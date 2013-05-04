/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.dheinrich.farmer.db

import scala.slick.driver.HsqldbDriver.simple._
import scala.slick.jdbc.meta.MTable
import Database.threadLocalSession
import scala.slick.lifted.DDL

trait AppDatabase {
  def DB = database
  lazy val database = Database.forURL("jdbc:hsqldb:file:db/testDB", user ="sa", password="", driver = "org.hsqldb.jdbcDriver")
  
  //initialize DB
  
  def isPresent[A](table:Table[A]) = {
    !MTable.getTables(None, None, Option(table.tableName), None).list.isEmpty
  }

  def initializeDB() = {  
    var tables:Seq[Table[_<:Any]] = Seq(Players, Staemme, Villages, Berichte, BerichtUnits, VillageBuildings, VillageUnits, VillagesResources)
    
    DB withTransaction {
      var ddl = tables map(_.ddl) reduce((a,b) => a ++ b)
                  
      if(tables.exists(t => isPresent(t))){
        if(tables.exists(t => !isPresent(t)))
          throw new Exception("Just some needed tables already exist");
      } else
        ddl create
    }
  }
}
