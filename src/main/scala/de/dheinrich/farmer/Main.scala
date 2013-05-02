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
import scala.slick.driver.HsqldbDriver.simple._
import Database.threadLocalSession
import de.dheinrich.farmer.db._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Promise
import scala.concurrent.Future
import de.dheinrich.farmer.json._
import java.io.File
import scalaz._
import Scalaz._
import Tag._
import java.text.SimpleDateFormat
import java.sql.Date
import java.sql.Timestamp

object Main extends AppDatabase {

  initializeDB

  val user = JsonMapper.mapper.readValue(new File("login.json"), classOf[UserLogin])
  val sessFuture = DieStaemme.login(user)

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

  def parseVillages(p: PlayerType, xml: Node @@ VillageOverview) = {
    val now = parseServerTime(xml)

    val idPattern = """label_text_(\d+)""".r
    val otherPattern = """(.+) \((\d+)\|(\d+)\).*""".r
    def parseVillageRow(n: Node): Village = {
      val idPattern(id) = (n \ "@id").text
      val otherPattern(name, x, y) = n.text
      return Village(id.toInt, Some(p.id), name, x.toInt, y.toInt, lastUpdate = now)
    }

    //find all tags of the following form: <span id="label_text_45048">London (409|693) K64</span>
    val villageNodes = xml \\ "span" filter (n => (n \ "@id").text startsWith "label_text")

    val villages = villageNodes map parseVillageRow

    villages map { v =>
      DB withSession {
        Villages updateNameOwner (v, now)
      }
    }
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

  def getXML[A](request: DieStaemme.Session => Future[Response] @@ A): Future[Node @@ A] = {
    for (
      s <- sessFuture;
      r <- request(s)
    ) yield Tag(HTML5Parser.loadXML(r.getResponseBody()))
  }

  def listImages = {
    val file = classOf[JsonVillage].getResourceAsStream("images.json")
    val images = JsonMapper.deserialize[JsonNode](file) map (_.asText) toArray

    images foreach println
  }

  def parseServerTime(xml: Node) = {
    val time = xml \\ "span" filter (n => (n \ "@id").text equals "serverTime") text
    val date = xml \\ "span" filter (n => (n \ "@id").text equals "serverDate") text

    parseDate(time + date, "HH:mm:ssdd/MM/yyyy")
  }

  def parseDate(text: String, pattern: String) = {
    val format = new SimpleDateFormat(pattern)
    new Timestamp(format.parse(text).getTime())
  }

  def populateDB = {
    val time = System.currentTimeMillis()
    val f = for (
      s <- sessFuture;
      sektors <- s.querryMap((0 until 900, 0 until 900))
    ) yield {
      println("time to load " + (System.currentTimeMillis() - time))

      val now = new Timestamp(new java.util.Date().getTime())

      DB withSession {
        Query(Villages).mutate(_.delete)
        Query(Players).mutate(_.delete)
        Query(Staemme).mutate(_.delete)
        println("füge Stämme hinzu")
        val s = sektors flatMap (_.data.allies) groupBy (_.id) map (_._2.head)
        Staemme.*.insertAll(s.map(_.toStamm).toSeq: _*)
        println("füge Spieler hinzu")
        val p = sektors flatMap (_.data.players) groupBy (_.id) map (_._2.head)
        Players.*.insertAll(p.map(_.toPlayer).toSeq: _*)
        println("füge Dörfer hinzu")
        val v = sektors flatMap (_.data.villages) groupBy (_.id) map (_._2.head)
        Villages.*.insertAll(v.map(_.toVillage.copy(lastUpdate = now)).toSeq: _*)
      }
    }

    f onFailure {
      case e: Exception => e.printStackTrace()
    }

    println("waiting...")
    Await.ready(f, 1 day)
    println("finished")

    def printSize(t: Table[_]) = println(Query(t).list.size)
    DB withSession {
      printSize(Staemme)
      printSize(Villages)
      printSize(Players)
    }
  }

  def saveGameDate(gdata: JsonGameData, now: Timestamp) = {
    DB withSession {
      println(s"saving buildings & speicher for village: ${gdata.village.name}")

      DB withSession {
        val builds = gdata.village.gebaeude map (b => VillageBuilding(gdata.village.id, b.typ, b.stufe))
        VillageBuildings.save(now, builds.toSeq: _*)

        import gdata.village.speicher
        VillagesResources.save(VillageResources(gdata.village.id, now, speicher.holz.amount, speicher.lehm.amount, speicher.eisen.amount))
      }
    }
  }

  def saveVillageUnits(villages: Iterable[Village]) = Future.sequence(villages map { vill =>
    for (xml2 <- getXML(_.villagePage(vill, Screens.Train))) yield {
      val now = parseServerTime(xml2)
      val units = parseUnitCounts(vill, xml2) map { u => VillageUnit(vill.id, u._1, u._2._2) }
      DB withSession {
        println(s"saving units for village: ${vill.name}")
        VillageUnits.save(now, units: _*)
      }
    }
  })

  def parseReportPageCount(xml: Node @@ ReportOverview) = (xml \\ "a" filter (n => (n \ "@class").text equals ("paged-nav-item"))).size + 1

  def parseReportIDsFromPage(xml: Node @@ ReportOverview) = {
    val table = xml \\ "table" filter (n => (n \ "@id").text equals "report_list") head
    val reports = (table \ "tbody" \\ "tr").tail.dropRight(1)

    def parseReportID(xml: Node) = {
      val tds = xml \\ "td"
      val date = parseDate(tds(1).text, "dd.MM.yy HH:mm")
      val id = (tds(0) \ "input" \ "@name").text.substring(3)

      (id.toInt, date)
    }
    reports map parseReportID
  }

  def parseReportVillages(xml: Node @@ Report) = {
    val Seq(attacker, defender) = xml \\ "span" \\ "@data-id" map (_.text.toInt)
    (attacker, defender)
  }

  private val resPattern = """ ([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)/([\d\.]+)""".r
  def parseReportResources(xml: Node @@ Report) = {
    val res = (xml \\ "table" filter (n => (n \ "@id").text equals "attack_results")) \ "tbody" \ "tr" \ "td" text;

    val opRes = Some(res) filter (!_.isEmpty())
    opRes map { r =>
      val p = (1 to 5) map resPattern.findFirstMatchIn(r).get.group map (_.replace(".", "").toInt)
      val Seq(holz, lehm, eisen, took, could) = p
      (holz.toInt, lehm.toInt, eisen.toInt)
    }
  }

  def updatePlayerVillages = {
    val futXml = getXML(_.screenRequest(Screens.VillageOverview))

    val futGameData = futXml map parseGameData
    val futTime = futXml map parseServerTime

    val futSavedGameData = for (now <- futTime; gdata <- futGameData) yield saveGameDate(gdata, now)

    val futVillages = for (xml <- futXml; gdata <- futGameData) yield parseVillages(gdata.player, xml)
    val futSavedUnits = futVillages map saveVillageUnits

    Await.ready(futSavedUnits, 1 day)
    Await.ready(futSavedGameData, 1 day)
  }

  def updateBerichte = {
    val futXml = getXML(_.reportOverview(0))
    val newest = DB withSession { Query(Berichte).sortBy(_.date desc) firstOption }

    val futCount = futXml map parseReportPageCount

    val futFirstIDs = futXml map parseReportIDsFromPage

    val futBerichte = for (count <- futCount) yield {
      def parseAndSave(futIDs: Future[Seq[(Int, Timestamp)]], page: Int): Future[Seq[Bericht]] = for (ids <- futIDs) yield {
        println(s"parsing report page $page")
        val toParse = ids filter (t => newest.map(t._2 after _.date).getOrElse(true))

        val dbBerichte = toParse map { t =>
          val futBericht = getXML(_.reportRequest(t._1))
          val futRes = futBericht map parseReportResources
          val futVil = futBericht map parseReportVillages

          val a = for (vil <- futVil; res <- futRes) yield {
            println(s"parsing report #${t._1}")
            val r = res getOrElse (0, 0, 0)
            Bericht(t._1, t._2, vil._1, vil._2, r._1, r._2, r._3)
          }
          for (ex <- a.either.left) { println(t._1); ex.printStackTrace() }
          a
        }

        val a = Future.sequence(dbBerichte)
        var b = a()

        if (ids.size == toParse.size && page + 1 < count) {
          val futXml = getXML(_.reportOverview(page + 1))
          val futNewIDs = futXml map parseReportIDsFromPage
          b = b ++ parseAndSave(futNewIDs, page + 1)()
        }
        b
      }

      parseAndSave(futFirstIDs, 0)
    }

    val futSave = futBerichte.flatten map { b =>
      println(s"adding ${b.size} new reports")
      DB withSession {
        Berichte.*.insertAll(b.toSeq: _*)
      }
    }

    Await.ready(futSave, 1 day)
  }

  def main(args: Array[String]): Unit = {
    //    populateDB

    //    updateBerichte
    //    updatePlayerVillages

    DB withSession {
      val q = for (b <- Berichte) yield (b.holz + b.lehm + b.eisen)
      println(q.list.sum)

      val q2 = Query(Berichte) groupBy (_.defenderID)

          println(q2.selectStatement)
          
//      q2 map {
//        case (vilID, bs) => {
//          val res = Query(VillagesResources).filter(_.villID is vilID).firstOption
//          val lu = res map (_.lastUpdate) getOrElse (new Timestamp(0))
//          val newer = bs.filter(_.date > lu)
//          val stolenRes = newer.map(b => (b.holz, b.lehm, b.eisen)).foldLeft((0, 0, 0))(_ |+| _)
//
//          println((vilID,stolenRes))
//        }
//      }

            val q3 = for (((vilID, bs), res) <- q2 leftJoin VillagesResources on (_._1 is _.villID)) yield{
              val qq = for(b <- bs if b.date > res.lastUpdate.?)) yield b
              
//              val holz = res.holz.?.getOrElse(0) - qq.map(_.holz).sum 
//              val lehm = res.lehm.?.getOrElse(0) - qq.map(_.lehm).sum 
//              val eisen = res.eisen.?.getOrElse(0) - qq.map(_.eisen).sum 
              
             qq.map(_.holz).sum 
            }
            
//            {
//              val newer = bs.filter(b => (res.lastUpdate.? isNull) || (b.date > res.lastUpdate))
//              val stolenRes = newer.map(_.holz).sum
//              (vilID, stolenRes)
//            }
      
      q3.list foreach println

    }

    //    DB withSession {
    //      val q = Query(Villages) groupBy { v =>
    //        (v.x / 20, v.y / 20)
    //      }
    //      val q2 = q map { case ((x, y), vills) => (vills map (_.lastUpdate) max, vills map (_.x) min, vills map (_.y) min) }
    //      println(q2.selectStatement)
    //      q2.list map(d=>(d._1, d._2 map(_ / 20), d._3  map(_ / 20))) foreach println
    //    }

    //     val q = Query(Villages) filter (_.ownerID isNull) sortBy (x => {
    //                val dx = x.x - v.x
    //                val dy = x.y - v.y
    //                (dx * dx + dy * dy)
    //              })
    //    Await.ready(f, 1 day)
    //    

  }
}