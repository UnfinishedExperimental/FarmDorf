package de.dheinrich.farmer

import scalaz._
import Tags._

trait Overview
trait MapScreen
trait VillageOverview
trait Train
trait ReportOverview
trait Report
trait Place

object Screens extends Enumeration {
  val Overview = V[Overview]("overview")
  val Map = V[MapScreen]("map")
  val VillageOverview = V[VillageOverview]("overview_villages")
  val Train = V[Train]("train")
  val Report = V[Report]("report")
  val Place = V[Place]("place")  

  private def V[T](name: String) = Tag[Value, T](Value(name))
}