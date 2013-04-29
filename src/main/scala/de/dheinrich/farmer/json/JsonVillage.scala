package de.dheinrich.farmer.json

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import de.dheinrich.farmer.Buildings

@JsonIgnoreProperties(value = Array("gebaeude"), ignoreUnknown = true)
case class JsonVillage(id: Int, name: String, var coordinate: (Int, Int),
  var speicher: Speicher) {

  var gebaeude: Iterable[Building] = _
  private val coordPattern = """(\d+)\|(\d+)""".r

  def setCoord(value: String) = {
    val coordPattern(x, y) = value
    coordinate = (x.toInt, y.toInt)
  }

  def setRes(values: Array[String]) = {
    def res(off: Int) = Resource(values(off).toInt, values(off + 1).toDouble)
    speicher = Speicher(res(0), res(2), res(4), values(6).toInt, values(7).toInt, values(8).toInt)
  }

  def setBuildings(values: Map[String, String]): Unit = {
    gebaeude = values map { case (t, s) => Building(Buildings.withName(t), s.toInt) } filter (_.stufe > 0)
  }
}

case class Resource(amount: Int, rate: Double)
case class Speicher(holz: Resource, lehm: Resource, eisen: Resource, max: Int, people: Int, maxPeople: Int)
case class Building(typ: Buildings.Value, stufe: Int)

//example json
//{
//    "id": 45048,
//    "name": "London",
//    "coord": "409|693",
//    "con": "K64",
//    "bonus": null,
//    "group": "0",
//    "res": [
//      188,
//      0.013112639828853,
//      185,
//      0.011273696515817,
//      270,
//      0.008333333333,
//      "1229",
//      "79",
//      "240"
//    ],
//    "buildings": {
//      "main": "5",
//      "farm": "1",
//      "storage": "2",
//      "place": "1",
//      "barracks": "1",
//      "church": "0",
//      "church_f": "1",
//      "smith": "1",
//      "wood": "4",
//      "stone": "3",
//      "iron": "1",
//      "market": "0",
//      "stable": "0",
//      "wall": "0",
//      "garage": "0",
//      "hide": "1",
//      "snob": "0",
//      "statue": "0"
//    }
//  }




  