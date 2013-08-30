package de.dheinrich.farmer.db

import java.sql.Date
import java.sql.Timestamp
import de.dheinrich.farmer.Units

case class Bericht(id: Int, date: Timestamp, attackerID: Int, defenderID: Int, holz: Int = 0, lehm: Int = 0, eisen: Int = 0, maxCapa: Int = 0)

trait BerichteComponent { this: DBProfile with VillagesComponent =>
  import profile.simple._

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
}
