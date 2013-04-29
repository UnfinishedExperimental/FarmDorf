package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import java.sql.Date

case class Bericht(id: Int, date: Date, attackerID: Int, defenderID: Int, holz: Int, lehm: Int, eisen: Int)

object Berichte extends Table[Bericht]("BERICHTE") {
  def id = column[Int]("ID", O.PrimaryKey)
  def attackerID = column[Int]("ATTACKER_ID")
  def defenderID = column[Int]("DEFENDER_ID")
  def date = column[Date]("DATE")
  def holz = column[Int]("HOLZ")
  def lehm = column[Int]("LEHM")
  def eisen = column[Int]("EISEN")

  def attacker = foreignKey("ATT_FK", attackerID, Villages)(_.id)
  def defender = foreignKey("DEF_FK", defenderID, Villages)(_.id)

  def * = id ~ date ~ attackerID ~ defenderID ~ holz ~ lehm ~ eisen <> (Bericht, Bericht.unapply _)
}