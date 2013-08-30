package de.dheinrich.farmer.db

import de.dheinrich.farmer.Units
import de.dheinrich.farmer.Buildings
import java.sql.Timestamp
import org.joda.time.DateTime

case class VillageBuilding(villID: Int, buildingType: Buildings.Value, value: Int)
case class VillageUnit(villID: Int, unitType: Units.Value, value: Int)
case class VillageResources(villID: Int, lastUpdate: DateTime, holz: Int, lehm: Int, eisen: Int)

trait VillageInfosComponent { this: DBProfile with VillagesComponent =>
  import profile.simple._

  object VillageBuildings extends Table[VillageBuilding]("VILLAGE_BUILDINGS") {

    implicit val mapper = MappedTypeMapper.base[Buildings.Value, Int](_.id, Buildings(_: Int))

    def villID: Column[Int] = column[Int]("VILL_ID")
    def buildingType = column[Buildings.Value]("TYPE")
    def value = column[Int]("HOLZ")

    def village = foreignKey("VILL_BUILD_FK", villID, Villages)(_.id)
    def idx1 = index("VB_IDX1", villID ~ buildingType, unique = true)

    def * = villID ~ buildingType ~ value <> (VillageBuilding, VillageBuilding.unapply _)

    def save(v: VillageBuilding, date: DateTime)(implicit session: Session) = {
      val a = Query(VillageBuildings) filter (_.villID is v.villID) filter (_.buildingType is v.buildingType) update v
      if (a == 0)
        * insert v
      Villages.buildingsUpdated(v.villID, date)
    }

    def of(vill: Village)(implicit session: Session) = for (b <- this if b.villID is vill.id) yield b
  }

  object VillageUnits extends Table[VillageUnit]("VILLAGE_UNITS") {

    implicit val mapper = MappedTypeMapper.base[Units.Value, Int](_.id, Units(_: Int))

    def villID: Column[Int] = column[Int]("VILL_ID")
    def unitType = column[Units.Value]("TYPE")
    def value = column[Int]("HOLZ")

    def village = foreignKey("VILL_UNIT_FK", villID, Villages)(_.id)
    def idx1 = index("VU_IDX1", villID ~ unitType, unique = true)

    def * = villID ~ unitType ~ value <> (VillageUnit, VillageUnit.unapply _)

    def save(date: DateTime, units: VillageUnit*)(implicit session: Session) = {
      units foreach { v =>
        val a = Query(VillageUnits) filter (_.villID is v.villID) filter (_.unitType is v.unitType) update v
        if (a == 0)
          * insert v
      }
      Villages.unitsUpdated(units(0).villID, date)
    }

    def of(vill: Village)(implicit session: Session) = for (b <- this if b.villID is vill.id) yield b
  }

  object VillagesResources extends Table[VillageResources]("VILLAGE_RESOURCES") {
    def villID = column[Int]("VILL_RES_ID", O.PrimaryKey)
    def lastUpdate = column[DateTime]("LAST_UPDATE")

    def holz = column[Int]("HOLZ")
    def lehm = column[Int]("LEHM")
    def eisen = column[Int]("EISEN")

    def village = foreignKey("VILL_FK", villID, Villages)(_.id)

    def * = villID ~ lastUpdate ~ holz ~ lehm ~ eisen <> (VillageResources, VillageResources.unapply _)

    def save(v: VillageResources)(implicit session: Session) = {
      val a = Query(VillagesResources) filter (_.villID is v.villID) update v
      if (a == 0)
        * insert v
    }
    
    def of(vill: Village)(implicit session: Session) = (for (b <- this if b.villID is vill.id) yield b) firstOption
  }
}
