package simulator.model

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Field, Position}
import simulator.interfaces.{PlayerMapping, State}

class GameState extends State {

  def checksum = 13

  // Board Information
  override def apply(pos: Position): Field = ???

  override def apply(x: Int, y: Int): Field = ???

  // Analytical Information (caller convenience)
  override def freeFields: Int = ???

  override def domination: PlayerMapping[Int] = ???

  override def longestStreet: PlayerMapping[Int] = ???

  // Player Information
  override def minionsLeft(player: PlayerColor): Int = ???

  override def capstonesLeft(player: PlayerColor): Int = ???

  // Static Information
  override def size: Int = ???

  override def minionsPerPlayer: Int = ???

  override def capstonesPerPlayer: Int = ???
}
