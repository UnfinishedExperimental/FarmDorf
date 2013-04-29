package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._

case class Village(id: Int, ownerID: Option[Int], name: String, x: Int, y: Int, points: Int = 0, mood: Int = 100)
//case class PlayerVillage buildings & rohstoffe

object Villages extends Table[Village]("VILLAGES") {
  def id = column[Int]("ID", O.PrimaryKey)
  def ownerID = column[Option[Int]]("OWNER_ID")
  def name = column[String]("NAME")
  def x = column[Int]("X")
  def y = column[Int]("Y")
  def points = column[Int]("POINTS")
  def mood = column[Int]("MOOD", O.Default(100))
  
  def pk = index("IDX_COORD", (x, y), unique = true)

  def * = id ~ ownerID ~ name ~ x ~ y ~ points ~ mood <> (Village, Village.unapply _)

  //Queries
  def byID(id: Int)(implicit session: Session) = Query(Villages).filter(_.id is id).firstOption

  private val ownedVillages = for (pid <- Parameters[Int]; v <- Villages if v.ownerID is pid) yield v
  def ofPlayer(player: Player) = ownedVillages(player.id)

  def save(v: Village)(implicit session: Session) = {
    val a = Query(Villages) filter (_.id is v.id) update v
    if (a == 0)
      * insert v
  }
}