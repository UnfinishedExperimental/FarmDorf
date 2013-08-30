package de.dheinrich.farmer

object  Units extends Enumeration{
//don't change order, it is used in the report parsing
  val SPEER = Value("spear")
  val SCHWERT = Value("sword")
  val AXT = Value("axe")
//  val BOGEN = Value("archer")
  val SPAEHER = Value("spy")
  val LREITER = Value("light")
//  val BREITER = Value("marcher")
  val SREITER = Value("heavy")
  val RAMBOCK = Value("ram")
  val KATAPULT = Value("catapult")
  val PALADIN = Value("knight")
  val ADEL = Value("snob")
}