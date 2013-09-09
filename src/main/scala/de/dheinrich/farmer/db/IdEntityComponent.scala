package de.dheinrich.farmer.db

import scala.slick.lifted.MappedProjection

trait IdEntity {
  val id: Int
}

trait IdEntityComponent { this: DBProfile =>
  import profile.simple._

  trait IdEntityTable[A <: IdEntity] {
    this: Table[A] =>

    def id: Column[Int]
    def * : MappedProjection[A, _]

    def byID(entity: A)(implicit session: Session): Option[A] = byID(entity.id)
    def byID(id: Int)(implicit session: Session): Option[A] = Query(this).filter(_.id is id).firstOption

    def save(entity: A)(implicit session: Session) = {
      val a = Query(this) filter (_.id is entity.id) update entity
      if (a == 0)
        * insert entity
    }

    def getOrInsert(entity: A)(implicit session: Session) = {
      byID(entity.id) match {
        case Some(vi) => vi
        case None => * insert entity; entity
      }
    }
  }
}