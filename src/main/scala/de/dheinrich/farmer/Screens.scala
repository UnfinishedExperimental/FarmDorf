package de.dheinrich.farmer

import scalaz._
import Tags._

trait Overview
trait Map
trait VillageOverview
trait Train

object Screens extends Enumeration {
  val Overview: Value @@ Overview = V("overview")
  val Map: Value @@ Map = V("map")
  val VillageOverview: Value @@ VillageOverview = V("overview_villages")
  val Train: Value @@ Train = V("train")

  private def V[T](name: String) = Tag[Value, T](Value(name))
}