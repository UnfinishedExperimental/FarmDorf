package de.dheinrich.farmer.db

import java.sql.Date

case class Player(id: Int, name: String, points: Int, stammID: Option[Int], schutz: Option[Date])

trait PlayersComponent { this: DBProfile =>
  import profile.simple._
  object Players extends Table[Player]("PLAYERS") {
    def id = column[Int]("ID", O.PrimaryKey)
    def name = column[String]("NAME")
    def points = column[Int]("PUNKTE")
    def stammID = column[Int]("STAMM_ID", O.Nullable)
    def schutz = column[Option[Date]]("SCHUTZ")

    def * = id ~ name ~ points ~ stammID.? ~ schutz <> (Player, Player.unapply _)

    def byStamm(stamm: Stamm)(implicit session: Session) = Query(Players) filter (_.stammID is stamm.id)

    def byName(name: String)(implicit session: Session) = Query(Players) filter (_.name is name) firstOption

    def save(v: Player)(implicit session: Session) = {
      val a = Query(Players) filter (_.id is v.id) update v
      if (a == 0)
        * insert v
    }
  }
}
