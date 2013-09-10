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
import de.dheinrich.farmer.{ DieStaemme => ds }
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
import shapeless._
import scala.annotation.tailrec
import java.io.FileWriter
import scala.collection.immutable.TreeMap

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

    (for (row <- unitRows) yield {
      val columns = row \\ "td"
      if (columns.size >= 3) {
        val typeText = (columns(0) \ "a" \ "@onclick").text
        val amountText = columns(columns.size - 2).text

        for (
          tm <- typePatter.findFirstMatchIn(typeText);
          um <- unitPatter.findFirstMatchIn(amountText)
        ) yield {
          val unitType = tm.group(1)
          val Seq(there, total) = (1 to 2) map { um.group(_).toInt }

          Units.withName(unitType) -> (there, total)
        }
      } else
        None
    }).flatten
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
    val time = (xml \\ "span" filter (n => (n \ "@id").text equals "serverTime") text) trim
    val date = (xml \\ "span" filter (n => (n \ "@id").text equals "serverDate") text) trim

    if (time.isEmpty || date.isEmpty) {
      //println(xml)
      Left()
    } else {

      Right(DateTimeFormat.forPattern("HH:mm:ss dd/MM/yyyy").parseDateTime(time.take(8) + " " + date.take(10)))
    }
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
      val parsed = parseUnitCounts(vill, xml2)
      val units = parsed map { u => VillageUnit(vill.id, u._1, u._2._2) }
      println(s"saving units for village: ${vill.name}")

      for (n <- now.right) DB withSession { VillageUnits.save(n, units: _*) }
      for (_ <- now.left) println("error parsing servertime while saving village units")
      (vill, parsed.toMap)
    }
  })

  val berichtPagePattern = """ \[(\d+)\] """.r
  def parseReportPageCount(xml: Node @@ ReportOverview) = {
    val last = (xml \\ "a" filter (n => (n \ "@class").text equals ("paged-nav-item"))).last.text
    val berichtPagePattern(n) = last
    n.toInt
  }

  def parseReportIDsFromPage(xml: Node @@ ReportOverview) = {
    val table = (xml \\ "table" filter (n => (n \ "@id").text equals "report_list")) head

    val reports = (table \\ "tr").tail.dropRight(1)

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
    //    if (vills.isEmpty)
    //      println("---------\n---------\n---------\n---------\n---------\n" + xml)
    val Seq(attacker, defender) = vills
    (attacker, defender)
  }

  private val resPattern = """([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)""".r
  private val capPattern = """([\d\.]+)/([\d\.]+)""".r

  def parseReportResources(xml: Node @@ Report) = {
    def parse(s: String) = s.filter(_ != '.').toInt

    val results = ((xml \\ "table" filter (n => (n \ "@id").text equals "attack_results")) \\ "td")

    if (results.isEmpty)
      (0, 0, 0, 0)
    else {
      val res = results(0).text
      val r = resPattern.findFirstMatchIn(res) match {
        case Some(m) =>
          val holz = m.group(1)
          val lehm = m.group(2)
          val eisen = m.group(3)
          (parse(holz), parse(lehm), parse(eisen))

        case None => (0, 0, 0)
      }

      val cap = results(1).text
      val c = capPattern.findFirstMatchIn(cap) map { m => parse(m.group(2)) } getOrElse 0

      (r._1, r._2, r._3, c)
    }
  }

  def parseReportSpyResources(xml: Node @@ Report) = {
    def parse(s: String) = s.filter(_ != '.').toInt

    val results = ((xml \\ "table" filter (n => (n \ "@id").text equals "attack_spy")) \\ "td")
    if (results.isEmpty)
      None
    else {
      val res = results.head.text
      resPattern.findFirstMatchIn(res) map { m =>
        val holz = m.group(1)
        val lehm = m.group(2)
        val eisen = m.group(3)
        (parse(holz), parse(lehm), parse(eisen))
      }
    }
  }

  def updatePlayerVillages = {
    val futXml = getXML(_.screenRequest(Screens.VillageOverview))

    val f = for (xml <- futXml) yield {
      val gdata = parseGameData(xml)
      for (now <- parseServerTime(xml).right) {
        //println(gdata)
        saveGameDate(gdata, now)
        saveVillageUnits(parseVillages(gdata.player, now, xml))
      }
    }

    f onFailure {
      case e: Exception => e.printStackTrace()
    }

    f()
  }

  sealed trait MovementType
  object Attack extends MovementType
  object Return extends MovementType
  object Cancel extends MovementType
  object Support extends MovementType
  object OrderedBack extends MovementType
  object SendBack extends MovementType

  case class VillageMovements(in: Seq[Movement], out: Seq[Movement])
  case class Movement(id: Int, from: Village, to: Village, moveType: MovementType, time: DateTime = null)

  def getTroopMovmentsFrom(vill: Village) = {
    val xml = getXML(_.villagePage(vill, Screens.Overview))
    xml map parseTroops
  }

  private val timePattern = """(\w+)(?: (.+))? um (.+) Uhr""".r
  def parseTroops(xml: Node @@ Overview) = {
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

      val parsedTime = DateTimeFormat.forPattern("HH:mm:ss:SSS").parseDateTime(time) + (1 hour)

      parsedDate.millisOfDay().setCopy(0) + parsedTime.millis
    }

    def parseCommand(xml: Node, isIncoming: Boolean) = {
      val img = (xml \ "img" \ "@src").text
      val tmp = xml \ "span" \ "a"
      val link = (tmp \ "@href").text
      val text = (tmp \ "span").text

      val imgPat = """/(\w+)\.png""".r
      val mType = imgPat.findFirstMatchIn(img).get.group(1) match {
        case "return" => Return
        case "attack" => Attack
        case "cancel" => Cancel
        case "support" => Support
        case "back" => OrderedBack
        case "other_back" => SendBack
      }

      //http://de92.die-staemme.de/game.php?village=129193&id=51713545&type=own&screen=info_command
      val linkPat = """village=(\d+)&id=(\d+)""".r
      val mat = linkPat.findFirstMatchIn(link).get

      val startVillage = DB withSession {
        val id = mat.group(1).toInt
        Villages byID id
      } get
      val commandID = mat.group(2).toInt

      //      Rückkehr von Barbarendorf (855|407) K48
      val destinie = if (isIncoming) {
        DB withSession {
          Query(Villages).filter(_.name is text).first
        }
      } else {
        val texPat = """\((\d+)\|(\d+)\)""".r
        val mat2 = texPat.findFirstMatchIn(text).get
        val Seq(x, y) = (1 to 2) map (i => mat2.group(i).toInt)

        DB withSession {
          Query(Villages).filter(v => v.x.is(x) && v.y.is(y)).first
        }
      }

      val returning = Seq(Return, Cancel, OrderedBack, SendBack) contains mType
      val (from, to) = if (returning) (destinie, startVillage) else (startVillage, destinie)
      Movement(commandID, from, to, mType)
    }
    //*[@id="show_incoming_units"]/div/form/table/tbody/tr[2]
    //*[@id="show_outgoing_units"]/div/table/tbody/tr[2]/
    def parseFromId(isIncoming: Boolean) = {
      val divID = if (isIncoming) "show_incoming_units" else "show_outgoing_units"

      val prepre = (xml \\ "div" filter (n => (n \ "@id").text equals divID))
      val preResults = prepre \\ "div"
      val weiter = if (isIncoming) preResults \\ "form" else preResults
      val results = weiter \\ "table" \\ "tr"

      if (results.isEmpty) {
        Nil
      } else
        (for (n <- results.tail) yield {
          val childs = n \\ "td"
          if (childs.size > 2) {
            val command = parseCommand(childs(0), isIncoming)
            val time = parseTime(childs(1))

            Some(command.copy(time = time))
          } else None
        }).flatten
    }
    val out = parseFromId(false)
    val in = parseFromId(true) map { m =>
      m.copy(from = m.to, to = m.from)
    }

    VillageMovements(in, out)
  }

  def plündern() = {
    getNewBerichte()

    val berichte = getAccBerichte().map(b => (b.villID, b.eisen + b.holz + b.lehm)).toMap.withDefaultValue(0)

    val me = DB withSession {
      Players byName user.userName get
    }

    val ownVillages = DB withSession {
      Villages ofPlayer me list
    }

    println(s"${me.name} has ${ownVillages.size} villages and ${me.points} points")

    val movsFut = ownVillages map getTroopMovmentsFrom

    val stillAttacking = Future.sequence {
      movsFut map {
        for (mov <- _) yield {
          mov.out map (_.to) filter (_.ownerID == 0) //only barbar villages
        }
      }
    } map (_.flatten)

    val allBarbar = DB withSession { Villages.allBarbars list }
    val villUnits = saveVillageUnits(ownVillages)().toMap
    val tree = new QuadTree[TreeStorage](allBarbar)

    val alreadyAttacking = stillAttacking().toSet

    case class AttackPackage(village: Village, units: Map[Units.Value, Int], stream: Stream[Village])

    val villagePackage = timed { () =>
      ownVillages map (v => DB withSession {

        val stream = tree.nearestStream(v.x, v.y)
          .filter(!alreadyAttacking.contains(_))

        val units = villUnits(v).
          map(u => (u._1, u._2._1)).
          toMap.withDefaultValue(0)

        AttackPackage(v, units, stream)
      })
    }("building packages")

    val LKAV_CAP = 80
    val LKAV_MIN = 30

    @tailrec
    def processVillages(vills: Seq[AttackPackage], rohstoffe: Map[Int, Int]) {
      import Units._

      val filtered = vills.filter(p => p.units(SPAEHER) > 0 && p.units(LREITER) >= LKAV_MIN)

      type foldType = (Map[Int, Int], List[AttackPackage])
      val (alterRoh, remaining) = filtered.foldLeft[foldType]((rohstoffe, Nil)) { (t, p) =>
        val (roh, packages) = t

        val target = p.stream.head

        val lkavCount = p.units(LREITER)
        val schicken = roh.get(target.id).
          map(_ / LKAV_CAP + 1).
          getOrElse(LKAV_MIN).
          min(p.units(LREITER))

        if (schicken >= LKAV_MIN) {
          val attackUnits = Map(SPAEHER -> 1, LREITER -> schicken)

          timed { () =>
            val attack = sessFuture.flatMap(_.prepareAttack(p.village, target, attackUnits))
            attack().confirm
          }(s"running attack from ${p.village.name} to coord (x: ${target.x}, y: ${target.y}) with $schicken LKAV")

          val newUnits = attackUnits.foldLeft(p.units)((map, e) => map.updated(e._1, map(e._1) - e._2)) //subtract used units
          val newRoh = roh.updated(target.id, roh(target.id) - schicken * LKAV_CAP)

          (newRoh, AttackPackage(p.village, newUnits, p.stream.tail) :: packages)
        } else
          (roh, AttackPackage(p.village, p.units, p.stream.tail) :: packages)
      }

      if (!remaining.isEmpty) processVillages(remaining, alterRoh)
    }

    timed { () =>
      processVillages(villagePackage, berichte)
    }("running attacks")

    Future.sequence(movsFut).apply
  }

  def getNewBerichte() {
    val futXml = getXML(_.reportOverview(0))
    val newest = DB withSession { Query(Berichte).sortBy(_.date.desc) firstOption }

    val futCount = futXml map parseReportPageCount

    val futFirstIDs = futXml map parseReportIDsFromPage

    val futBerichte = (for (count <- futCount) yield {
      println(s"parsing $count pages of reports!")

      def parseAndSave(futIDs: Future[Seq[(Int, DateTime)]], page: Int): Future[Seq[(Future[Bericht], Future[Option[VillageResources]])]] = for (ids <- futIDs) yield {
        val toParse = ids filter (t => newest.map(_.date < t._2).getOrElse(true))

        val dbBerichte = toParse map { t =>
          val futBericht = getXML(_.reportRequest(t._1))

          val futRes = futBericht map parseReportResources
          val futVil = futBericht map parseReportVillages
          val futSpyRes = futBericht map parseReportSpyResources

          val b = for (vil <- futVil; res <- futSpyRes) yield {
            res map { r => VillageResources(vil._2, t._2, r._1, r._2, r._3) }
          }

          val a = for (vil <- futVil; res <- futRes) yield {
            println(s"parsed report #${t._1}  ${t._2}")
            Bericht(t._1, t._2, vil._1, vil._2, res._1, res._2, res._3, res._4)
          }
          for (ex <- a.either.left) { ex.printStackTrace(); println(t._1); println(futBericht()) }
          (a, b)
        }

        if (ids.size == toParse.size && page + 1 < count) {
          val futXml = getXML(_.reportOverview(page + 1))
          val futNewIDs = futXml map parseReportIDsFromPage
          dbBerichte ++ parseAndSave(futNewIDs, page + 1)()
        } else
          dbBerichte
      }

      parseAndSave(futFirstIDs, 0)
    }).flatten

    futBerichte onFailure {
      case e: Exception => e.printStackTrace()
    }

    val futSave = futBerichte map { s =>
      for ((futureBericht, futRes) <- s) {
        for (
          b <- futureBericht;
          r <- futRes
        ) {
          DB withSession {
            Berichte.* insert b
            r foreach { VillagesResources save _ }
          }
        }
      }
    }

    val berichte = futBerichte.map(x => x.map(_._1)).flatMap(Future.sequence(_))
    val endFut = berichte map { b => println(s"added ${b.size} new reports") }

    futSave()
    endFut()
  }

  def getAccBerichte() = DB withSession {
    val spyedBar = (for (
      (r, v) <- VillagesResources innerJoin Villages on (_.villID is _.id) if v.ownerID isNull
    ) yield (v.id, r)) groupBy (_._1)

    val lastUpdates = spyedBar.map {
      case (id, d) =>
        (id, d.map(_._2.lastUpdate).max)
    }
    val res = for (
      (lu, r) <- lastUpdates innerJoin VillagesResources on ((l, r) => l._1 === r.villID && l._2 === r.lastUpdate)
    ) yield r

    res.list
  }

  def main(args: Array[String]): Unit = {

    //   

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

    //    DB withSession {
    //      for (user <- Players byName user.userName) {
    //        val ownVillages = (Villages ofPlayer user) list;
    //        println(s"${user.name} has ${ownVillages.size} villages and ${user.points} points")

    //val barbar = Query(Villages).filter(_.ownerID isNull).list
    //val tree = new QuadTree[TreeStorage](barbar)

    //println(s"there are ${barbar.size} Barbar villages!")
    //        ownVillages
    //
    //        val first = ownVillages.head
    //        println(first)
    //
    //        var l: List[Village] = Nil
    //        var l2: List[Village] = Nil
    //
    //        val take = 10
    //
    //        timed { () =>
    //          val stream = tree.nearestStream(first.x, first.y)
    //          l = stream.take(take).toList
    //        }("nearest search", 10)
    //
    //        timed { () =>
    //          l2 = barbar.sortBy { b =>
    //            val w = first.x - b.x
    //            val h = first.y - b.y
    //
    //            w * w + h * h
    //          } take take
    //        }("all just sort", 10)
    //
    //        println(l2 equals l)

    //        val target = Query(Villages).filter(v => (v.x === 858) && (v.y === 409)).first
    //
    //        for (session <- sessFuture) {
    //          val fut = session.prepareAttack(first, target, Map(Units.SPEER -> 10))
    //
    //          fut onSuccess {
    //            case attack =>
    //              println(s"attacking from ${first.name} village: ${target.name}")
    //              attack.confirm()
    //          }
    //          
    //          fut onFailure {
    //            case t:Throwable => t.printStackTrace
    //            case a => println(a)
    //          }
    //        }
    //
    //        Thread.sleep(1000000)
    //        println("end")

    //        val f = for (
    //          v <- ownVillages
    //        ) yield getTroopMovmentsFrom(v) map { m =>
    //          val (i, o) = m.out.flatMap(_.to.ownerID).partition(_ == user.id)
    //          val inc = i.size + m.in.size
    //          val out = o.size
    //          println(s"Movments for ${v.name}:\n\tincoming = $inc\n\toutgoing = $out")
    //          val attacks = m.in.
    //            filter(_.moveType == Attack).
    //            map { a => s"\tAttack from ${a.from.name} arriving at: ${a.time}" }.
    //            mkString("\n")
    //          println(attacks)
    //          2
    //        }
    //
    //        val ff = Future.sequence(f)
    //        ff()
    //        
    //
    //        //        val tree = timed { () =>
    //        //          val vills = Query(Villages).list
    //        //          new QuadTree[TreeStorage](vills)
    //        //        }("ini quadtree")
    //        //
    //        //        timed { () =>
    //        //          timed { () =>
    //        //            val points = ownVillages.map(v => (v.x, v.y))
    //        //            val neighbours = tree.radiusSearch(points, 100)
    //        //          }("search", 100)prin
    //        //        }("total search", 10)
    //      }
    //    }

        val me = DB withSession {
          Players byName user.userName get
        }
    
        val ownVillages = DB withSession {
          Villages ofPlayer me list
        }
    
        val loggedAttacks = scala.collection.mutable.Set[Movement]()
    
        while (true) {
          try {
            var lastUpdate = DateTime.now
            while (true) {
              val attacks = plündern().flatMap(_.in).filter(_.moveType == Attack)
              val toLog = attacks.filter(loggedAttacks.add(_))
              val now = DateTime.now
              logHeader(now, toLog.size)
              toLog.sortBy(_.to.name) foreach (logAttack(_, lastUpdate, now))
    
              @tailrec
              def time: Long = {
                val moves = (Future.sequence(ownVillages map getTroopMovmentsFrom)).apply.flatMap(_.out)
                val r = moves.filter(_.moveType == Return)
    
                if (r.size > 0) {
                  val next = r.map(_.time).sorted.head
                  (next.getMillis() - DateTime.now.getMillis()) + 500
                } else {
                  val a = moves.filter(_.moveType == Attack)
                  if (a.size > 0) {
                    val next = a.map(_.time).sorted.head
                    val t = (next.getMillis() - DateTime.now.getMillis()) + 500
    
                    println("waiting for attacks to happen")
                    printAndSleepTime(t)
    
                    time
                  } else
                    (5 minutes).millis
                }
              }
    
              lastUpdate = DateTime.now
    
              println("waiting for returning troops")
              printAndSleepTime(time)
            }
          } catch {
            case _: Throwable =>
              printAndSleepTime((15 minutes).millis)
          }
        }
    //    printUnits()
    //populateDB
    //    getNewBerichte()
    //    
    //    DB withSession {
    //      val n = getAccBerichte map { b => b.eisen + b.holz + b.lehm }
    //      println(n.sum)
    //    }

  }

  val logFile = "attackLog.txt"

  val runTimes = {
    import Units._
    TreeMap(9 -> SPAEHER, 10 -> LREITER, 11 -> SREITER,
      18 -> SPEER, 18 -> AXT, 22 -> SCHWERT,
      30 -> RAMBOCK, 30 -> KATAPULT, 35 -> ADEL)
  }

  def logHeader(now: DateTime, count: Int) {
    if (count > 0) {
	val text = count + " new attacks"
	val cmd = Array("sh", "-c", "echo \"" + text + "\" | festival --tts");
        Runtime.getRuntime().exec(cmd);
	println(text)

      val fw = new FileWriter(logFile, true)
      try {
        fw.write(s"\nLoging $count new attacks at: ${now}\n")
      } finally fw.close()
    }
  }

  def logAttack(a: Movement, last: DateTime, now: DateTime) {
    val dist = QuadTree.dist(a.to, a.from)

    val lower = (new Duration(now, a.time).getStandardMinutes() / dist).ceil.toInt
    val upper = (new Duration(last, a.time).getStandardMinutes() / dist).ceil.toInt min 35

    val lastKey = runTimes.from(upper).firstKey
    val possibleTypes = runTimes.range(lower, lastKey + 1)

    val uType = possibleTypes.values.mkString(", ")

    val fw = new FileWriter(logFile, true)
    try {
      fw.write(s"${a.time}\ton: ${a.to.toShortString}\tfrom: ${a.from.toShortString}\twith: [$uType]\n")
    } finally fw.close()
  }

  def printAndSleepTime(ms: Long) {
    if (ms > 0) {
      val formater = DateTimeFormat.forPattern("HH:mm:ss")
      println(s"Wakeup next at ${formater.print(DateTime.now + (ms.toDuration))} Uhr")
      Thread sleep ms
    }
  }

  def printUnits() {
    val me = DB withSession {
      Players byName user.userName get
    }

    val ownVillages = DB withSession {
      Villages ofPlayer me list
    }

    val units = saveVillageUnits(ownVillages)().toMap

    for ((v, u) <- units) {
      println(s"Units in ${v.name}:")

      val lines = u.filter(e => e._2._2 > 0).map(e => s"\t${e._1}\t\t${e._2._1}\t/\t${e._2._2}")
      println(lines.mkString("\n"))
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





