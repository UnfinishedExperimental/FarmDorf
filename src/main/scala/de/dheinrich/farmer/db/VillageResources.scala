package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import java.sql.Date
import de.dheinrich.farmer.Units
import de.dheinrich.farmer.Buildings

case class VillageBuilding(villID: Int, buildingType: Buildings.Value, value: Int)
object VillageBuildings extends Table[VillageBuilding]("VILLAGE_UNITS") {

  implicit val mapper = MappedTypeMapper.base[Buildings.Value, Int](_.id, Buildings(_:Int))

  def villID = column[Int]("VILL_ID")
  def buildingType = column[Buildings.Value]("TYPE")
  def value = column[Int]("HOLZ")

  def village = foreignKey("VILL_FK", villID, Villages)(_.id)
  def idx1 = index("idx1", villID ~ buildingType, unique = true)

  def * = villID ~ buildingType ~ value <> (VillageBuilding, VillageBuilding.unapply _)
}

case class VillageUnit(villID: Int, unitType: Units.Value, value: Int)
object VillageUnits extends Table[VillageUnit]("VILLAGE_UNITS") {

  implicit val mapper = MappedTypeMapper.base[Units.Value, Int](_.id, Units(_:Int))

  def villID = column[Int]("VILL_ID")
  def unitType = column[Units.Value]("TYPE")
  def value = column[Int]("HOLZ")

  def village = foreignKey("VILL_FK", villID, Villages)(_.id)
  def idx1 = index("idx1", villID ~ unitType, unique = true)

  def * = villID ~ unitType ~ value <> (VillageUnit, VillageUnit.unapply _)
}

case class VillageResources(villID: Int, lastUpdate: Date, holz: Int, lehm: Int, eisen: Int)
object VillagesResources extends Table[VillageResources]("VILLAGE_RESOURCES") {
  def villID = column[Int]("VILL_ID", O.PrimaryKey)
  def lastUpdate = column[Date]("LAST_UPDATE")

  def holz = column[Int]("HOLZ")
  def lehm = column[Int]("LEHM")
  def eisen = column[Int]("EISEN")

  def village = foreignKey("VILL_FK", villID, Villages)(_.id)

  def * = villID ~ lastUpdate ~ holz ~ lehm ~ eisen <> (VillageResources, VillageResources.unapply _)
}