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
    def parseVillageRow(n: Node) = {
      val idPattern(id) = (n \ "@id").text
      val otherPattern(name, x, y) = n.text
      Village(id.toInt, Some(p.id), name, x.toInt, y.toInt, lastUpdate = now)
    }

    //find all tags of the following form: <span id="label_text_45048">London (409|693) K64</span>
    val villageNodes = xml \\ "span" filter (n => (n \ "@id").text startsWith "label_text")

    val villages = villageNodes map parseVillageRow

    villages map { v =>
      DB withSession {
        Query(Villages).filter(_.id is v.id) firstOption match {
          case Some(c) => c
          case _ => Villages.*.insert(v); v
        }
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
    new Date(format.parse(text).getTime())
  }

  def populateDB = {
    val time = System.currentTimeMillis()
    val f = for (
      s <- sessFuture;
      sektors <- s.querryMap((0 until 900, 0 until 900))
    ) yield {
      println("time to load " + (System.currentTimeMillis() - time))

      val now = new Date(new java.util.Date().getTime())

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

  def saveGameDate(gdata: JsonGameData, now: Date) = {
    DB withSession {
      println(s"saving buildings & speicher for village: ${gdata.village.name}")
      for (b <- gdata.village.gebaeude) {
        val building = VillageBuilding(gdata.village.id, b.typ, b.stufe)
        VillageBuildings.save(building, now)
      }

      val spei = gdata.village.speicher
      VillagesResources.save(VillageResources(gdata.village.id, now, spei.holz.amount, spei.lehm.amount, spei.eisen.amount))
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

  private val resPattern = """ (\d+)\s+(\d+)\s+(\d+)\s+([\d\.]+)/([\d\.]+)""".r
  def parseReportResources(xml: Node @@ Report) = {
    val res = (xml \\ "table" filter (n => (n \ "@id").text equals "attack_results")) \ "tbody" \ "tr" \ "td" text
    val resPattern(holz, lehm, eisen, took, could) = res
    (holz.toInt, lehm.toInt, eisen.toInt)
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

  def main(args: Array[String]): Unit = {
    //    populateDB

    val futXml = getXML(_.reportOverview(0))

    val newest = DB withSession { Query(Berichte).sortBy(_.date) firstOption }
    val futCount = futXml map parseReportPageCount

    val futFirstIDs = futXml map parseReportIDsFromPage

    val futBerichte = for (count <- futCount) yield {
      def parseAndSave(futIDs: Future[Seq[(Int, Date)]], page: Int): Future[Seq[Bericht]] = for (ids <- futIDs) yield {
        println(s"parsing report page $page")
        val toParse = ids filter (t => newest.map(_.date before t._2).getOrElse(true))

        val dbBerichte = toParse map { t =>
          val futBericht = getXML(_.reportRequest(t._1))
          val futRes = futBericht map parseReportResources
          val futVil = futBericht map parseReportVillages
         
          val a = for (vil <- futVil; res <- futRes) yield {            
            println(s"parsing report #${t._1}")
            Bericht(t._1, t._2, vil._1, vil._2, res._1, res._2, res._3)
          }
          for(ex <- a.either.left) {println(t._1);println(futBericht())}
          a
        }

        val a = Future.sequence(dbBerichte)
        val b = a()

        if (ids.size == toParse.size && page + 1 < count) {
          val futXml = getXML(_.reportOverview(page + 1))
          val futNewIDs = futXml map parseReportIDsFromPage
          b ++ parseAndSave(futNewIDs, page + 1)()
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

    //     val q = Query(Villages) filter (_.ownerID isNull) sortBy (x => {
    //                val dx = x.x - v.x
    //                val dy = x.y - v.y
    //                (dx * dx + dy * dy)
    //              })
    //    Await.ready(f, 1 day)
    //    

  }
}