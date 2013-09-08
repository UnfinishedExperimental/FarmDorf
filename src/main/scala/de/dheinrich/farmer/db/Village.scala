package de.dheinrich.farmer.db

import java.sql.Timestamp
import org.joda.time.DateTime

case class Village(id: Int, ownerID: Option[Int], name: String, x: Int, y: Int, points: Int = 0, mood: Int = 100,
  lastUpdate: DateTime = new DateTime(0), lastUnitUp: DateTime = new DateTime(0), lastBuildingsUp: DateTime = new DateTime(0))

trait VillagesComponent { this: DBProfile =>
  import profile.simple._

  implicit val dateTypeMapper = MappedTypeMapper.base[DateTime, Timestamp](
    { dt => new Timestamp(dt.getMillis()) }, 
    { ts => new DateTime(ts.getTime()) } 
    )

  object Villages extends Table[Village]("VILLAGES") {

    def id = column[Int]("ID", O.PrimaryKey)
    def ownerID = column[Option[Int]]("OWNER_ID")
    def name = column[String]("NAME")

    def lastUpdate = column[DateTime]("LAST_UPDATE")
    def lastUpdateUnits = column[DateTime]("LAST_UPDATE_UNITS")
    def lastUpdateBuildings = column[DateTime]("LAST_UPDATE_BUILDINGS")

    def x = column[Int]("X")
    def y = column[Int]("Y")

    def points = column[Int]("POINTS")
    def mood = column[Int]("MOOD", O.Default(100))

    def pk = index("IDX_COORD", (x, y), unique = true)

    def * = id ~ ownerID ~ name ~ x ~ y ~ points ~ mood ~ lastUpdate ~ lastUpdateUnits ~ lastUpdateBuildings <> (Village, Village.unapply _)

    //Queries
    def byID(id: Int)(implicit session: Session) = Query(Villages).filter(_.id is id).firstOption

    private def ownedVillages(pid: Int) = for (v <- Villages if v.ownerID is pid) yield v
    def ofPlayer(player: Player) = ownedVillages(player.id)
    
    def allBarbars() = Query(Villages) filter(_.ownerID isNull)

    def save(v: Village)(implicit session: Session) = {
      val a = Query(Villages) filter (_.id is v.id) update v
      if (a == 0)
        * insert v
    }

    def getOrInsert(v: Village)(implicit session: Session) = {
      byID(v.id) match {
        case Some(vi) => vi
        case None => * insert v; v
      }
    }

    def unitsUpdated(vid: Int, date: DateTime)(implicit session: Session) = {
      val q = for (v <- Villages if v.id is vid) yield v.lastUpdateUnits
      q update date
    }

    def buildingsUpdated(vid: Int, date: DateTime)(implicit session: Session) = {
      val q = for (v <- Villages if v.id is vid) yield v.lastUpdateBuildings
      q update date
    }
  }
}
