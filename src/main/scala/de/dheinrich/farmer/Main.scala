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

object Test {
  val name = "King Henry 8th"
  val pwd = "084021442"
  val pwdHash = "4802ae031febb916d20a61005a5a3f86c93c0738"
  val server = "de92"
}

object Main extends AppDatabase {

  initializeDB

  val user = JsonMapper.mapper.readValue(new File("login.json"), classOf[UserLogin])
  val sessFuture = DieStaemme.login(user)

  //      val gamePattern = """game_data = (\{.+\})""".r
  //      val gamedata = (xml \\ "script").view flatMap { n: Node =>
  //        gamePattern.findFirstIn(n.text) map { a =>
  //          val gamePattern(data) = a
  //          JsonMapper.deserialize[JsonGameData](data)
  //        }
  //      } head

  def ownVillages = for (xml <- screenXML(DieStaemme.dorfUebersicht)) yield {

    val idPattern = """label_text_(\d+)""".r
    def parseVillageRow(n: Node) =
      {
        val idPattern(id) = (n \ "@id").text
        id.toInt
      }

    //find all tags of the following form: <span id="label_text_45048">London (409|693) K64</span>
    val villageNodes = xml \\ "span" filter (n => (n \ "@id").text startsWith "label_text")

    val villageIDs = villageNodes map parseVillageRow

    villageIDs map { v =>
      DB withSession {
        Query(Villages).filter(_.id is v) first;
      }
    }
  }

  def getUnitCountFromVillage(v: Village): Future[Seq[(String, (Int, Int))]] = {
    val typePatter = """return UnitPopup\.open\(event, '(\w+)'\)""".r
    val unitPatter = """(\d+)/(\d+)""".r

    for (xml <- screenVilXML(v, "train")) yield {
      val unitRows = xml \\ "tr" filter (n => (n \ "@class").toString startsWith "row")
      for (row <- unitRows) yield {
        val columns = row \\ "td"
        val typePatter(unitType) = (columns(0) \ "a" \ "@onclick").toString
        val unitPatter(there, total) = columns(columns.size - 2).text
        unitType -> (there.toInt, total.toInt)
      }
    }
  }

  def screenXML(descr: Any) = {
    for (
      s <- sessFuture;
      r <- s.screenRequest(descr.toString())
    ) yield HTML5Parser.loadXML(r.getResponseBody())
  }

  def screenVilXML(v: Village, descr: Any) = {
    for (
      s <- sessFuture;
      r <- s.villagePage(v, descr.toString())
    ) yield HTML5Parser.loadXML(r.getResponseBody())
  }

  def listImages = {
    val file = classOf[JsonVillage].getResourceAsStream("images.json")
    val images = JsonMapper.deserialize[JsonNode](file) map (_.asText) toArray

    images foreach println
  }

  //  def getKBestFarms(v: Village, k: Int, filter: Iterable[JsonMapVillage] => Iterable[JsonMapVillage]) = {
  //    val sekx = v.x / 20
  //    val seky = v.y / 20
  //
  //    for (
  //      s <- sessFuture;
  //      sektors <- s.querryMap((sekx, seky))
  //    ) yield {
  //      val barbars = sektors flatMap (_.data.villages) filter (_.owner == None) groupBy (_.id) map (_._2.head)
  //
  //      val best = filter(barbars)
  //      if (best.size < k)
  //        null
  //      else
  //        best.take(k)
  //    }
  //
  //  }

  def populateDB = {
    val time = System.currentTimeMillis()
    val f = for (
      s <- sessFuture;
      sektors <- s.querryMap((0 until 900, 0 until 900))
    ) yield {
      println("time to load " + (System.currentTimeMillis() - time))

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
        Villages.*.insertAll(v.map(_.toVillage).toSeq: _*)
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

    val f = for (s <- sessFuture) yield {

      DB withSession {
        val player = Players.byName(s.user.userName).get
        val villages = Villages.ofPlayer(player)
        
        for (v <- villages) {
          println(v)
          
          val q = Query(Villages) filter (_.ownerID isNull) sortBy (x => {
            val dx = x.x - v.x
            val dy = x.y - v.y
            (dx * dx + dy * dy)
          })

          val b = q.take(50).list
          b foreach { c =>
            val dx = c.x - v.x
            val dy = c.y - v.y
            println(Math.sqrt(dx * dx + dy * dy))
          }
        }
      }
    }

    Await.ready(f, 1 day)
    //    populateDB   
    //    Await.ready(f, 1 day)
  }

}