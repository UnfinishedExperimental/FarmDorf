package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import java.sql.Timestamp

private object Time{
  val old = new Timestamp(0)
}

case class Village(id: Int, ownerID: Option[Int], name: String, x: Int, y: Int, points: Int = 0, mood: Int = 100,
  lastUpdate: Timestamp = Time.old, lastUnitUp: Timestamp = Time.old, lastBuildingsUp: Timestamp = Time.old) {
  def buildings(implicit session: Session) = Query(VillageBuildings) filter (_.villID is id) list
  def units(implicit session: Session) = Query(VillageUnits) filter (_.villID is id) list
  def resources(implicit session: Session) = Query(VillagesResources) filter (_.villID is id) list
}

object Villages extends Table[Village]("VILLAGES") {
  def id = column[Int]("ID", O.PrimaryKey)
  def ownerID = column[Option[Int]]("OWNER_ID")
  def name = column[String]("NAME")

  def lastUpdate = column[Timestamp]("LAST_UPDATE")
  def lastUpdateUnits = column[Timestamp]("LAST_UPDATE_UNITS")
  def lastUpdateBuildings = column[Timestamp]("LAST_UPDATE_BUILDINGS")

  def x = column[Int]("X")
  def y = column[Int]("Y")

  def points = column[Int]("POINTS")
  def mood = column[Int]("MOOD", O.Default(100))

  def owner = foreignKey("village_player_fk", ownerID, Players)(_.id.?) 
  def pk = index("IDX_COORD", (x, y), unique = true)

  def * = id ~ ownerID ~ name ~ x ~ y ~ points ~ mood ~ lastUpdate ~ lastUpdateUnits ~ lastUpdateBuildings <> (Village, Village.unapply _)

  //Queries
  def byID(id: Int)(implicit session: Session) = Query(Villages).filter(_.id is id).firstOption

  private val ownedVillages = for (pid <- Parameters[Int]; v <- Villages if v.ownerID is pid) yield v
  def ofPlayer(player: Player) = ownedVillages(player.id)

  def save(v: Village)(implicit session: Session) = {
    val a = Query(Villages) filter (_.id is v.id) update v
    if (a == 0)
      * insert v
  }
  
  def unitsUpdated(vid:Int, date:Timestamp)(implicit session: Session) = (for(v <- Villages if v.id is vid)yield v.lastUpdateUnits) update(date)
  def buildingsUpdated(vid:Int, date:Timestamp)(implicit session: Session) = (for(v <- Villages if v.id is vid)yield v.lastUpdateBuildings) update(date)
}