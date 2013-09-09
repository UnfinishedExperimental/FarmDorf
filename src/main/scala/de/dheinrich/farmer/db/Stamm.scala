package de.dheinrich.farmer.db

case class Stamm(id: Int, name: String, tag: String) extends IdEntity

trait StaemmeComponent extends IdEntityComponent { this: DBProfile with PlayersComponent =>
  import profile.simple._

  object Staemme extends Table[Stamm]("STAEMME") with IdEntityTable[Stamm] {
    def id = column[Int]("ID", O.PrimaryKey)
    def name = column[String]("NAME")
    def tag = column[String]("TAG")
    def * = id ~ name ~ tag <> (Stamm, Stamm.unapply _)

    def byPlayer(p: Player)(implicit session: Session) = p.stammID map { id =>
      Query(Staemme) filter (_.id is id) first
    }

    def playersOf(stamm: Stamm)(implicit s: Session) = Players.byStamm(stamm)
  }
}