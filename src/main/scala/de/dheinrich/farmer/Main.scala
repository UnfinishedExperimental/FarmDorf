package de.dheinrich.farmer

import com.fasterxml.jackson.databind.ObjectMapper
import scala.collection.JavaConversions._
import dispatch._
import Defaults._
import com.ning.http.client.Response
import com.ning.http.client.Cookie
import scala.collection.mutable.Buffer
import scala.xml.Node
import json.JsonVillage
import json.JsonPlayer
import json.JsonGameData
import json.JsonGameData
import java.nio.file.Files
import com.sun.org.apache.bcel.internal.util.ClassLoader
import json.JsonVillage
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.JsonNode
import json.JsonMapSektor
import de.dheinrich.farmer.db.AppDatabase
import scala.slick.driver.HsqldbDriver.simple._
import Database.threadLocalSession
import de.dheinrich.farmer.db.Staemme
import de.dheinrich.farmer.db.Players
import de.dheinrich.farmer.db.Villages
import scala.concurrent.Await
import scala.concurrent.duration._
import de.dheinrich.farmer.db.Stamm
import de.dheinrich.farmer.json.JsonMapper
import java.io.File
import de.dheinrich.farmer.db.Village
import scala.concurrent.Promise
import scala.concurrent.Future
import scalaz._
import Scalaz._
import de.dheinrich.farmer.json.JsonMapVillage
import java.text.SimpleDateFormat
import java.sql.Date
import de.dheinrich.farmer.db.Village
import de.dheinrich.farmer.db.Player
import de.dheinrich.farmer.json.JsonPlayer
import de.dheinrich.farmer.db.VillageBuildings
import de.dheinrich.farmer.db.VillageBuilding
import de.dheinrich.farmer.db.VillageResources
import de.dheinrich.farmer.db.VillagesResources
import de.dheinrich.farmer.db.VillageUnits
import de.dheinrich.farmer.db.VillageUnit

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

  def ownVillages(p: PlayerType) = for (xml <- screenXML(Screens.VillageOverview)) yield parseVillages(p, xml)

  def parseVillages(p: PlayerType, xml: Node) = {
    val now = parseServerTime(xml)

    val idPattern = """label_text_(\d+)""".r
    val otherPattern = """(.+) \((\d+)|(\d+)\)""".r
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

  def getUnitCountFromVillage(v: Village) = for (xml <- screenVilXML(v, Screens.Train)) yield parseUnitCounts(v, xml)

  def parseUnitCounts(v: Village, xml: Node) = {
    val typePatter = """return UnitPopup\.open\(event, '(\w+)'\)""".r
    val unitPatter = """(\d+)/(\d+)""".r

    val unitRows = xml \\ "tr" filter (n => (n \ "@class").toString startsWith "row")
    for (row <- unitRows) yield {
      val columns = row \\ "td"
      val typePatter(unitType) = (columns(0) \ "a" \ "@onclick").toString
      val unitPatter(there, total) = columns(columns.size - 2).text
      Units.withName(unitType) -> (there.toInt, total.toInt)
    }
  }

  def screenXML(descr: Screens.Value) = {
    for (
      s <- sessFuture;
      r <- s.screenRequest(descr)
    ) yield HTML5Parser.loadXML(r.getResponseBody())
  }

  def screenVilXML(v: Village, descr: Screens.Value) = {
    for (
      s <- sessFuture;
      r <- s.villagePage(v, descr)
    ) yield HTML5Parser.loadXML(r.getResponseBody())
  }

  def listImages = {
    val file = classOf[JsonVillage].getResourceAsStream("images.json")
    val images = JsonMapper.deserialize[JsonNode](file) map (_.asText) toArray

    images foreach println
  }

  def parseServerTime(xml: Node) = {
    val time = xml \\ "span" filter (n => n \ "@id" equals "serverTime") toString
    val date = xml \\ "span" filter (n => n \ "@id" equals "serverDate") toString

    val format = new SimpleDateFormat("HH:mm:ssdd/MM/yyyy")
    new Date(format.parse(time + date).getTime())
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

  def main(args: Array[String]): Unit = {

//    populateDB
    val f = for (s <- sessFuture) yield {

      DB withSession {
        val player = Players.byName(s.user.userName).get

        for (
          xml <- screenXML(Screens.VillageOverview);
          nowO <- parseServerTime(xml).success;
          gdata <- parseGameData(xml).success
        ) {
//          println(gdata.village)
//          val un:List[VillageUnit] = Query(VillageUnits) filter(_.villID is gdata.village.id) list;
//          un foreach println
          for (b <- gdata.village.gebaeude) {
            val building = VillageBuilding(gdata.village.id, b.typ, b.stufe)
            VillageBuildings.save(building, nowO)
          }
          
          val spei = gdata.village.speicher          
          VillagesResources.save(VillageResources(gdata.village.id, nowO, spei.holz.amount, spei.lehm.amount, spei.eisen.amount))

          for (
            vill <- parseVillages(gdata.player, xml);
            xml2 <- screenVilXML(vill, Screens.Train);
            now <- parseServerTime(xml2).success;
            units <- parseUnitCounts(vill, xml2)
          ) {
            val unit = VillageUnit(gdata.village.id, units._1, units._2._2)
            VillageUnits.save(unit, now)
          }
        }
      }
    }

    Await.ready(f, 1 day)

    //     val q = Query(Villages) filter (_.ownerID isNull) sortBy (x => {
    //                val dx = x.x - v.x
    //                val dy = x.y - v.y
    //                (dx * dx + dy * dy)
    //              })
    //    Await.ready(f, 1 day)
  }

}