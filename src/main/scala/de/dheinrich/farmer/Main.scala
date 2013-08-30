package de.dheinrich.farmer

import com.fasterxml.jackson.databind.ObjectMapper
import scala.collection.JavaConversions._
import dispatch._
import Defaults._
import com.ning.http.client.Response
import com.ning.http.client.Cookie
import scala.collection.mutable.Buffer
import scala.xml.Node
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.JsonNode
import json.JsonMapSektor
import de.dheinrich.farmer.db._
import scala.concurrent.Promise
import scala.concurrent.Future
import de.dheinrich.farmer.json._
import java.io.File
import scalaz._
import Scalaz._
import Tag._
import java.text.SimpleDateFormat
import scala.slick.driver.HsqldbDriver
import de.dheinrich.farmer.spatial.QuadTree
import de.dheinrich.farmer.spatial.ArrayStorage
import de.dheinrich.farmer.spatial.TreeStorage
import java.sql.Timestamp
import scala.xml.Elem
import com.github.nscala_time.time.Imports._

object Main {

  val dal = new DataAccessLayer(HsqldbDriver)
  import dal._
  import dal.profile.simple._
  import Database.threadLocalSession

  val DB = Database.forURL("jdbc:hsqldb:file:db/testDB", user = "sa", password = "", driver = "org.hsqldb.jdbcDriver")

  DB.withTransaction { initializeDB(_: Session) }

  val user = JsonMapper.mapper.readValue(new File("login.json"), classOf[UserLogin])
  lazy val sessFuture = DieStaemme.login(user)

  def parseGameData(xml: Node) = {
    val gamePattern = """game_data = (\{.+\})""".r
    (xml \\ "script").view flatMap { n: Node =>
      gamePattern.findFirstIn(n.text) map { a =>
        val gamePattern(data) = a
        JsonMapper.deserialize[JsonGameData](data)
      }
    } head
  }

  type PlayerType = { val id: Int; val name: String }

  def parseVillages(p: PlayerType, now: DateTime, xml: Node @@ VillageOverview) = {
    val idPattern = """label_text_(\d+)""".r
    val otherPattern = """(.+) \((\d+)\|(\d+)\).*""".r
    def parseVillageRow(n: Node): Village = {
      val idPattern(id) = (n \ "@id").text
      val otherPattern(name, x, y) = n.text
      Village(id.toInt, Some(p.id), name, x.toInt, y.toInt, lastUpdate = now)
    }

    //find all tags of the following form: <span id="label_text_45048">London (409|693) K64</span>
    val villageNodes = xml \\ "span" filter (n => (n \ "@id").text startsWith "label_text")

    val villages = villageNodes map parseVillageRow
    DB.withSession { villages.map(Villages getOrInsert _) }
  }

  def parseUnitCounts(v: Village, xml: Node @@ Train) = {
    val typePatter = """return UnitPopup\.open\(event, '(\w+)'\)""".r
    val unitPatter = """(\d+)/(\d+)""".r

    val unitRows = xml \\ "tr" filter (n => (n \ "@class").text startsWith "row")
    for (row <- unitRows) yield {
      val columns = row \\ "td"
      val typePatter(unitType) = (columns(0) \ "a" \ "@onclick").text
      val unitPatter(there, total) = columns(columns.size - 2).text
      Units.withName(unitType) -> (there.toInt, total.toInt)
    }
  }

  def getXML[A](request: DieStaemme.Session => Future[Node] @@ A): Future[Node @@ A] = {
    for (
      s <- sessFuture;
      xml <- request(s)
    ) yield Tag(xml)
  }

  def listImages = {
    val file = classOf[JsonVillage].getResourceAsStream("images.json")
    val images = JsonMapper.deserialize[JsonNode](file) map (_.asText) toArray

    images foreach println
  }

  def parseServerTime(xml: Node) = {
    val time = xml \\ "span" filter (n => (n \ "@id").text equals "serverTime")
    val date = xml \\ "span" filter (n => (n \ "@id").text equals "serverDate")

    if (time.isEmpty || date.isEmpty)
      Left()
    else
      Right(DateTimeFormat.forPattern("HH:mm:ssdd/MM/yyyy").parseDateTime(time.text + date.text))
  }
  //
  def populateDB = {
    val time = System.currentTimeMillis()

    val f = for (
      s <- sessFuture;
      sektors <- s.queryWorld
    ) yield {
      println("time to load " + (System.currentTimeMillis() - time))

      val now = DateTime.now
      DB.withSession {
        Query(Villages).mutate(_.delete)
        Query(Players).mutate(_.delete)
        Query(Staemme).mutate(_.delete)

        println("füge Stämme hinzu")
        val s = sektors flatMap (_.data.allies) groupBy (_.id) map (_._2.head.toStamm)
        if (!s.isEmpty)
          Staemme.*.insertAll(s.toSeq: _*)

        println("füge Spieler hinzu")
        val p = sektors flatMap (_.data.players) groupBy (_.id) map (_._2.head.toPlayer)
        if (!p.isEmpty)
          Players.*.insertAll(p.toSeq: _*)

        println("füge Dörfer hinzu")
        val v = sektors flatMap (_.data.villages) groupBy (_.id) map (_._2.head.toVillage.copy(lastUpdate = now))
        if (!v.isEmpty)
          Villages.*.insertAll(v.toSeq: _*)
      }
    }

    f onFailure {
      case e: Exception => e.printStackTrace()
    }

    println("waiting...")
    f()
    println("finished")
    DB.withSession {
      def printSize(t: Table[_]) = println(Query(t).list.size)
      printSize(Staemme)
      printSize(Players)
      printSize(Villages)
    }
  }

  def saveGameDate(gdata: JsonGameData, now: DateTime) = {
    println(s"saving buildings & speicher for village: ${gdata.village.name}")
    DB.withSession {
      for (b <- gdata.village.gebaeude) {
        val building = VillageBuilding(gdata.village.id, b.typ, b.stufe)
        VillageBuildings.save(building, now)
      }

      val spei = gdata.village.speicher
      VillagesResources.save(VillageResources(gdata.village.id, now,
        spei.holz.amount, spei.lehm.amount, spei.eisen.amount))
    }
  }

  def saveVillageUnits(villages: Iterable[Village]) = Future.sequence(villages map { vill =>
    for (xml2 <- getXML(_.villagePage(vill, Screens.Train))) yield {
      val now = parseServerTime(xml2)
      val units = parseUnitCounts(vill, xml2) map { u => VillageUnit(vill.id, u._1, u._2._2) }
      println(s"saving units for village: ${vill.name}")

      for (n <- now.right) VillageUnits.save(n, units: _*)
      for (_ <- now.left) println("error parsing servertime while saving village units")
    }
  })

  def parseReportPageCount(xml: Node @@ ReportOverview) = (xml \\ "a" filter (n => (n \ "@class").text equals ("paged-nav-item"))).size + 1

  def parseReportIDsFromPage(xml: Node @@ ReportOverview) = {
    val table = xml \\ "table" filter (n => (n \ "@id").text equals "report_list") head
    val reports = (table \ "tbody" \\ "tr").tail.dropRight(1)

    def parseReportID(xml: Node) = {
      val tds = xml \\ "td"
      val date = DateTimeFormat.forPattern("dd.MM.yy HH:mm").parseDateTime(tds(1).text)
      val id = (tds(0) \ "input" \ "@name").text.substring(3)

      (id.toInt, date)
    }
    reports map parseReportID
  }

  def parseReportVillages(xml: Node @@ Report) = {
    val vills = xml \\ "span" \\ "@data-id" map (_.text.toInt)
    if (vills.isEmpty)
      println("---------\n---------\n---------\n---------\n---------\n" + xml)
    val Seq(attacker, defender) = vills
    (attacker, defender)
  }

  private val resPattern = """ ([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)/([\d\.]+)""".r
  def parseReportResources(xml: Node @@ Report) = {
    def parse(s: String) = s.filter(_ != '.').toInt

    val results = (xml \\ "table" filter (n => (n \ "@id").text equals "attack_results"))
    if (results.isEmpty) {
      (0, 0, 0)
    } else {
      val res = results \ "tbody" \ "tr" \ "td" text

      resPattern.findFirstMatchIn(res) match {
        case Some(m) =>

          //val Seq(holz, lehm, eisen, took, could) = for (i <- 1 to m.groupCount) yield m.group(i)
          val holz = m.group(1)
          val lehm = m.group(2)
          val eisen = m.group(3)
          (parse(holz), parse(lehm), parse(eisen))

        case None => (0, 0, 0)
      }
    }
  }

  def updatePlayerVillages = {
    val futXml = getXML(_.screenRequest(Screens.VillageOverview))

    val f = for (xml <- futXml) yield {
      val gdata = parseGameData(xml)
      for (now <- parseServerTime(xml).right) {
        println(gdata)
        saveGameDate(gdata, now)
        saveVillageUnits(parseVillages(gdata.player, now, xml))
      }
    }

    f onFailure {
      case e: Exception => e.printStackTrace()
    }

    f()
  }

  case class Movement(id: Int, from: Village, to: Village, time: DateTime = null)

  def getOutgoingTroopsFrom(vill: Village) = {
    val xml = getXML(_.villagePage(vill, Screens.Overview))
    xml map parseOutgoingTroops
  }

  private val timePattern = """(\w+)(?: (.+))? um (.+) Uhr""".r
  def parseOutgoingTroops(xml: Node @@ Overview) = {

    val now = parseServerTime(xml).right.get

    def parseTime(xml: Node) = {
      val timePattern(prefix, date, time) = xml.text

      val parsedDate =
        if (date == null) {
          prefix match {
            case "morgen" => now + (1 day)
            case "heute" => now
          }
        } else
          DateTimeFormat.forPattern("dd.MM.").parseDateTime(date)

      val parsedTime = DateTimeFormat.forPattern("HH:mm:ss:SSS").parseDateTime(time)

      parsedDate.millisOfDay().setCopy(0) + parsedTime.millis
    }

    def parseCommand(xml: Node) = {
      val img = (xml \ "img" \ "@src").text
      val tmp = xml \ "span" \ "a"
      val link = (tmp \ "@href").text
      val text = (tmp \ "span").text

      val returning = img.contains("return")

      //http://de92.die-staemme.de/game.php?village=129193&id=51713545&type=own&screen=info_command
      val linkPat = """village=(\d+)&id=(\d+)""".r
      val mat = linkPat.findFirstMatchIn(link).get

      val attacker = DB withSession {
        val id = mat.group(1).toInt
        Villages byID id
      } get
      val commandID = mat.group(2).toInt

      //      Rückkehr von Barbarendorf (855|407) K48
      val texPat = """\((\d+)\|(\d+)\)""".r
      val mat2 = texPat.findFirstMatchIn(text).get
      val Seq(x, y) = (1 to 2) map (i => mat2.group(i).toInt)

      val defender = DB withSession {
        Query(Villages).filter(v => v.x.is(x) && v.y.is(y)).first
      }

      val (from, to) = if (returning) (defender, attacker) else (attacker, defender)
      Movement(commandID, from, to)
    }

    //*[@id="show_outgoing_units"]/div/table/tbody/tr[2]/td[1]
    val results = ((xml \\ "div" filter (n => (n \ "@id").text equals "show_outgoing_units")) \ "div" \ "table" \ "tbody" \\ "tr")

    if (results.isEmpty)
      Nil
    else
      for (n <- results.tail) yield {
        val childs = n \\ "td"

        val command = parseCommand(childs(0))
        val time = parseTime(childs(1))

        command.copy(time = time)
      }
  }

  def main(args: Array[String]): Unit = {
    //populateDB

    //    val futXml = getXML(_.reportOverview(0))
    //
    //    val newest = DB withSession { Query(Berichte).sortBy(_.date) firstOption }
    //    val futCount = futXml map parseReportPageCount
    //
    //    val futFirstIDs = futXml map parseReportIDsFromPage
    //
    //    val futBerichte = (for (count <- futCount) yield {
    //      println(s"parsing $count pages of reports!")
    //
    //      def parseAndSave(futIDs: Future[Seq[(Int, Date)]], page: Int): Future[Seq[Future[Bericht]]] = for (ids <- futIDs) yield {
    //        val toParse = ids //filter (t => newest.map(_.date before t._2).getOrElse(true))
    //
    //        val dbBerichte = toParse map { t =>
    //          val futBericht = getXML(_.reportRequest(t._1))
    //          futBericht(); //strange multi-threading bug, xml has to be resolved otherwise villageParsing fails with only half parsed html
    //          //perhaps it is because of the cookies. perhaps only one request at a time should be possible
    //          val futRes = futBericht map parseReportResources
    //          val futVil = futBericht map parseReportVillages
    //
    //          val a = for (vil <- futVil; res <- futRes) yield {
    //            println(s"parsed report #${t._1}")
    //            Bericht(t._1, t._2, vil._1, vil._2, res._1, res._2, res._3)
    //          }
    //          for (ex <- a.either.left) { ex.printStackTrace(); println(t._1); println(futBericht()) }
    //          a
    //        }
    //
    //        if (ids.size == toParse.size && page + 1 < count) {
    //          val futXml = getXML(_.reportOverview(page + 1))
    //          val futNewIDs = futXml map parseReportIDsFromPage
    //          dbBerichte ++ parseAndSave(futNewIDs, page + 1)()
    //        } else
    //          dbBerichte
    //      }
    //
    //      parseAndSave(futFirstIDs, 0)
    //    }).flatten
    //
    //    futBerichte  onFailure {
    //      case e: Exception => e.printStackTrace()
    //    }
    //    
    //    val futSave = futBerichte map { s =>
    //      for (futureBericht <- s; bericht <- futureBericht) {
    //        //DB withSession { println("test"); Berichte.* insert bericht; println(s"saved report ${bericht.id}") }
    //      }
    //    }
    //
    //    val berichte = futBerichte.map(Future.sequence(_)).flatten
    //    val endFut = berichte map { b => println(s"added ${b.size} new reports") }
    //
    //    Await.ready(futSave, 1 day)
    //    Await.ready(endFut, 1 day)

    //updatePlayerVillages
    //    val f = for (s <- sessFuture) yield {
    //      DB withSession {
    //        val player = Players byName s.user.userName get
    //
    //        println(player)
    //
    //        val myVills = Query(Villages) filter (_.ownerID is player.id) list
    //        
    //        println(s"you have ${myVills.size} villages")
    //        myVills foreach println
    //
    //        val v = myVills(0)
    //
    //        val q = Query(Villages) filter (_.ownerID isNull) sortBy (x => {
    //          val dx = x.x - v.x
    //          val dy = x.y - v.y
    //          (dx * dx + dy * dy)
    //        })
    //
    //        q map println
    //      }
    //    }
    //    f onFailure {
    //      case e: Exception => e.printStackTrace()
    //    }
    //    Await.ready(f, 1 day)

    DB withSession {
      for (user <- Players byName user.userName) {
        val ownVillages = (Villages ofPlayer user) list;
        println(s"${user.name} has ${ownVillages.size} villages and ${user.points} points")

        val movements = ownVillages map (v => (v, getOutgoingTroopsFrom(v)))

        val f = for (v <- ownVillages) yield {
          //          val m = getOutgoingTroopsFrom(v)()
          for (m <- getOutgoingTroopsFrom(v)) yield {
            val inc = m flatMap (_.to.ownerID) filter (_ == user.id) size
            val out = m.size - inc
            s"Movments for ${v.name}:\n\tincoming = $inc\n\toutgoing = $out"
          }
        }

        val ff = Future.sequence(f map (_ map println))
        ff()

        //        val tree = timed { () =>
        //          val vills = Query(Villages).list
        //          new QuadTree[TreeStorage](vills)
        //        }("ini quadtree")
        //
        //        timed { () =>
        //          timed { () =>
        //            val points = ownVillages.map(v => (v.x, v.y))
        //            val neighbours = tree.radiusSearch(points, 100)
        //          }("search", 100)
        //        }("total search", 10)
      }
    }
  }

  def timed[A](f: () => A)(text: String = "Took", times: Int = 1): A = {
    val time = System.currentTimeMillis()
    var iter = 0
    var a: A = null.asInstanceOf[A]
    while (iter < times) {
      a = f()
      iter = iter + 1
    }
    println(text + s": ${System.currentTimeMillis() - time} ms")
    a
  }
}

