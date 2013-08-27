package de.dheinrich.farmer.db

case class Stamm(id: Int, name: String, tag: String)

trait StaemmeComponent { this: DBProfile with PlayersComponent =>
  import profile.simple._
  
  object Staemme extends Table[Stamm]("STAEMME") {
    def id = column[Int]("ID", O.PrimaryKey)
    def name = column[String]("NAME")
    def tag = column[String]("TAG")
    def * = id ~ name ~ tag <> (Stamm, Stamm.unapply _)

    def byPlayer(p: Player)(implicit session: Session) = p.stammID map { id =>
      Query(Staemme) filter (_.id is id) first
    }

    def playersOf(stamm: Stamm)(implicit s: Session) = Players.byStamm(stamm)

    def save(v: Stamm)(implicit session: Session) = {
      val a = Query(Staemme) filter (_.id is v.id) update v
      if (a == 0)
        * insert v
    }
  }
}