package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._

case class Stamm(id: Int, name: String, tag: String){
  def players(implicit s:Session) = Players.byStamm(this)
}
object Staemme extends Table[Stamm]("STAEMME") {
  def id = column[Int]("ID", O.PrimaryKey)
  def name = column[String]("NAME")
  def tag = column[String]("TAG")
  def * = id ~ name ~ tag <> (Stamm, Stamm.unapply _)
  
  def byPlayer(p:Player)(implicit session: Session) = p.stammID map {id =>
    Query(Staemme) filter (_.id is id) first
  }
  
  def save(v: Stamm)(implicit session: Session) = {
    val a = Query(Staemme) filter (_.id is v.id) update v
    if (a == 0)
      * insert v
  }
}