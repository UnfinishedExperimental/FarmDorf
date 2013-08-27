/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.dheinrich.farmer.db

import scala.slick.jdbc.meta.MTable
import scala.slick.lifted.DDL
import scala.slick.driver.ExtendedProfile

trait DBProfile {
  val profile: ExtendedProfile
}

class DataAccessLayer(override val profile: ExtendedProfile) extends DBProfile
  with BerichteComponent
  with PlayersComponent
  with StaemmeComponent
  with VillagesComponent
  with VillageInfosComponent {

  import profile.simple._

  def initializeDB(implicit session: Session) = {
    var tables: Seq[Table[_ <: Any]] = Seq(Players, Staemme, Villages, Berichte, VillageBuildings, VillageUnits, VillagesResources)

    var ddl = tables map (_.ddl) reduce ((a, b) => a ++ b)

    if (tables.exists(t => isPresent(t))) {
      if (tables.exists(t => !isPresent(t)))
        throw new Exception("Just some needed tables already exist");
    } else
      ddl create
  }

  private def isPresent[A](table: Table[A])(implicit session: Session) = {
    !MTable.getTables(None, None, Option(table.tableName), None).list.isEmpty
  }
}
