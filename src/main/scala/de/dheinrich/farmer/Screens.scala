package de.dheinrich.farmer

object Screens extends Enumeration {
  val Overview = Value("overview")
  val Map = Value("map")
  val VillageOverview = Value("overview_villages")
  val Train = Value("train")
  
  Buildings.values foreach(b=> Value(b.toString()))
}