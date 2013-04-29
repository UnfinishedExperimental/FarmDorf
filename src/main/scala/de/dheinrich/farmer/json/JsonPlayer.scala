package de.dheinrich.farmer.json

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import de.dheinrich.farmer.db.Player

@JsonIgnoreProperties(ignoreUnknown = true)
case class JsonPlayer(id: Int, name: String, ally_id: Int, points: Int, rank: Int, incomings: Int) {
  def toPlayer = Player(id, name, points, Some(ally_id), None)
}