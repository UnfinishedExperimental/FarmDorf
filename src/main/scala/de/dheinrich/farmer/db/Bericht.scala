package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import java.sql.Date
import java.sql.Timestamp
import de.dheinrich.farmer.Units

case class Bericht(id: Int, date: Timestamp, attackerID: Int, defenderID: Int, holz: Int = 0, lehm: Int = 0, eisen: Int = 0, maxCapa: Int = 0)

object Berichte extends Table[Bericht]("BERICHTE") {
  def id = column[Int]("ID", O.PrimaryKey)
  def attackerID = column[Int]("ATTACKER_ID")
  def defenderID = column[Int]("DEFENDER_ID")
  def date = column[Timestamp]("DATE")
  def holz = column[Int]("HOLZ")
  def lehm = column[Int]("LEHM")
  def eisen = column[Int]("EISEN")
  def max = column[Int]("MAX")

  def attacker = foreignKey("ATT_FK", attackerID, Villages)(_.id)
  def defender = foreignKey("DEF_FK", defenderID, Villages)(_.id)

  def * = id ~ date ~ attackerID ~ defenderID ~ holz ~ lehm ~ eisen ~ max <> (Bericht, Bericht.unapply _)
}

case class BerichtUnit(berichtID: Int, attacker: Boolean, unitType: Units.Value, had: Int, lost: Int)
object BerichtUnits extends Table[BerichtUnit]("REPORT_UNITS") {
  implicit val mapper = MappedTypeMapper.base[Units.Value, Int](_.id, Units(_: Int))

  def berichtID = column[Int]("VILL_ID")
  def attacker = column[Boolean]("ATTACKER")
  def unitType = column[Units.Value]("TYPE")
  def had = column[Int]("HAD")
  def lost = column[Int]("LOST")

  def village = foreignKey("REPORT_UNIT_FK", berichtID, Berichte)(_.id)
  def idx1 = index("RU_IDX1", berichtID ~ attacker ~ unitType, unique = true)

  def * = berichtID ~ attacker ~ unitType ~ had ~ lost <> (BerichtUnit, BerichtUnit.unapply _)
}