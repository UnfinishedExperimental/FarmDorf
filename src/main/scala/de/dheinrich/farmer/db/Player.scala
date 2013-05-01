package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import java.sql.Date

case class Player(id: Int, name: String, points: Int, stammID: Option[Int], schutz: Option[Date])
{
  def stamm(implicit session: Session) = Staemme.byPlayer(this)
}

object Players extends Table[Player]("PLAYERS") {
  def id = column[Int]("ID", O.PrimaryKey)
  def name = column[String]("NAME")
  def points = column[Int]("PUNKTE")
  def stammID = column[Option[Int]]("STAMM_ID")
  def schutz = column[Option[Date]]("SCHUTZ")
  
  def stamm = foreignKey("player_stamm_fk", stammID, Staemme)(_.id.?) 
   
  def * = id ~ name ~ points ~ stammID ~ schutz <> (Player, Player.unapply _)
  
  def byStamm(stamm:Stamm)(implicit session: Session) = Query(Players) filter(_.stammID is stamm.id)
  
  def byName(name:String)(implicit session: Session) = Query(Players) filter(_.name is name) firstOption
  
  def save(v: Player)(implicit session: Session) = {
    val a = Query(Players) filter (_.id is v.id) update v
    if (a == 0)
      * insert v
  }
}