package de.dheinrich.farmer

object Buildings extends Enumeration {
  val Hauptgebaeude = Value("main")
  
  val VersammlungsPlatz = Value("place")
  val Staue = Value("statue")
  val Kirche = Value("church")
  val Erste_Kirche = Value("church_f")
  
  val Bauernhof = Value("farm")
  val Speicher = Value("storage")
  
  val Stall = Value("stable")
  val Werkstatt = Value("garage")
  val Kaserne = Value("barracks")
  val Schmiede = Value("smith")
  val Adelshof = Value("snob")
  
  val Marktplatz = Value("market")
  val Holzfaeller = Value("wood")
  val Lehmgrube = Value("stone")
  val Eisenmine = Value("iron")
  
  val Versteck = Value("hide")
  val Wall = Value("wall")
}