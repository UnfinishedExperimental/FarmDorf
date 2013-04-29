package de.dheinrich.farmer.json

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import de.dheinrich.farmer.db.Village

@JsonIgnoreProperties(ignoreUnknown = true)
case class JsonGameData(player: JsonPlayer, village: JsonVillage) {
  import village._
  def getVillage = Village(id, Some(player.id), name, coordinate._1, coordinate._2)
}

//example json
//{
//  "player": {...},
//  "nav": {
//    "parent": 2
//  },
//  "village": {...},
//  "link_base": "\/game.php?village=45048&amp;screen=",
//  "link_base_pure": "\/game.php?village=45048&screen=",
//  "csrf": "3531",
//  "world": "de92",
//  "market": "de",
//  "RTL": false,
//  "version": "17757 8.11",
//  "majorVersion": "8.11",
//  "screen": "overview_villages",
//  "mode": null
//}