package de.dheinrich.farmer.json

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import scala.collection.JavaConversions._
import java.sql.Date
import de.dheinrich.farmer.db.Stamm
import de.dheinrich.farmer.db.Player
import de.dheinrich.farmer.db.Village

case class JsonMapSektor(data: JsonMapData, tiles: Array[Array[Int]], x: Int, y: Int)

@JsonIgnoreProperties(ignoreUnknown = true)
case class JsonMapData(x: Int, y: Int) {
  var allies: Iterable[JsonAllie] = _
  var players: Iterable[JsonMapPlayer] = _
  var villages: Iterable[JsonMapVillage] = _

  def setAllies(node: JsonNode) = allies = parse(node, JsonAllie(_))

  def setPlayers(node: JsonNode) = players = parse(node, JsonMapPlayer(_))

  def parse[C <: { def parse(i: Array[String]) }](node: JsonNode, constructor: Int => C) =
    parseAsArray(node) map { d => val a = constructor(d._1); a.parse(d._2 toArray); a } toIterable

  def setVillages(node: JsonNode) {
    val vs = for (
      row <- parseAsNodes(node);
      village <- row._2
    ) yield {
      val localX = row._1
      val localY = village._1

      val v = JsonMapVillage(x + localX, y + localY)
      v.parse(village._2 toArray)
      v
    }
    villages = vs toIterable
  }

  private def parseAsArray(node: JsonNode) = node.fields() map { f =>
    val id = f.getKey().toInt
    val attributs = f.getValue map (_.asText())

    (id, attributs)
  }

  private def parseAsNodes(node: JsonNode) = node.fields() map { f =>
    val id = f.getKey().toInt
    val attributs = parseAsArray(f.getValue)

    (id, attributs)
  }

  override def toString() = {
    val a = allies.mkString(",")
    val p = players.mkString(",")
    val v = villages.mkString(",")
    s"JsonMapData(allies:[$a],players:[$p],villages:[$v],x:$x,y:$y)"
  }
}

case class JsonAllie(id: Int) {
  //  {name: v[0], points: v[1], tag: v[2]}
  var name: String = _
  var tag: String = _
  var points: Int = _

  def parse(values: Array[String]) = {
    name = values(0)
    points = values(1).replace(".", "").toInt
    tag = values(2)
  }

  def toStamm = Stamm(id, name, tag)

  override def toString() = s"JsonAllie(id:$id,name:$name,tag:$tag,points:$points)"
}

case class JsonMapPlayer(id: Int) {
  //  {name: v[0], points: v[1], ally: v[2], newbie: v[3], sleep: v[4]}
  var name: String = _
  var points: Int = _
  var allyID: Int = _
  var newbie: Option[Date] = None
  var sleep: Option[String] = None

  def toPlayer = Player(id, name, points, Some(allyID) filter (_ != 0), newbie)

  def parse(values: Array[String]) = {
    name = values(0)
    points = values(1).replace(".", "").toInt
    allyID = values(2).toInt

    val date = """(?:am (\d+)\.(\d+)\.|heute) um (\d+):(\d+) Uhr""".r
    val matches = date findFirstMatchIn values(3)
    val dateParts = matches map (1 to 4 map _.group)

    newbie = if (values.size > 4)
      dateParts map { p =>
        val Seq(month, day, hour, minute) = p

        val now = new java.util.Date()

        val year = if (month != null && month.toInt < now.getMonth()) now.getYear() + 1 else now.getYear()
        val accMonth = Option(month).map(_.toInt).getOrElse(now.getMonth())
        val accDay = Option(day).map(_.toInt).getOrElse(now.getDay())

        val date = new Date(year, accMonth, accDay)
        date.setMinutes(minute.toInt)
        date.setHours(hour.toInt)
        date
      }
    else None

    sleep = if (values.size > 4)
      values(4) match {
        case "0" => None
        case v => Option(v)
      }
    else None
  }
  override def toString() = s"JsonMapPlayer(id:$id,name:$name,points:$points," +
    s"allyID:$allyID,newbie:$newbie,sleep:$sleep)"
}

case class JsonMapVillage(x: Int, y: Int) {
  var id: Int = _
  var imageID: Int = _
  var name: String = _
  var points: Int = _
  var owner: Option[Int] = _
  var mood: Int = _
  var bonus: Option[String] = None

  def toVillage = Village(id, owner, name, x, y, points, mood)

  //{id: v[0], img: v[1], name: v[2], points: v[3], owner: v[4], mood: v[5], bonus: v[6]
  def parse(values: Array[String]) = {
    id = values(0).toInt
    imageID = values(1).toInt
    name = values(2)
    points = values(3).replace(".", "").toInt
    owner = Some(values(4).toInt) filter (_ != 0)
    mood = values(5).toInt
    bonus = if (values.size > 6) Option(values(6)) else None
  }
  override def toString() = s"JsonMapVillage(id:$id,imageID:$imageID,name:$name,points:$points," +
    s"owern:$owner,mood:$mood,bonus:$bonus,x:$x,y:$y)"
}





