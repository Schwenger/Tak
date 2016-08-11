package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Field, Position}

/**
  * Contains all vital information for the game's current state.
  */
trait State {

  val size: Int

  // Board Information
  def apply(pos: Position): Field
  def apply(x: Int, y: Int): Field

  // Analytical Information (caller convenience)
  def freeFields: Int
  def domination: PlayerMapping[Int]
  def longestStreet: PlayerMapping[Int]

  // Player Information
  def minionsLeft(player: PlayerColor): Int
  def capstonesLeft(player: PlayerColor): Int

  def piecesLeft(player: PlayerColor): Int = minionsLeft(player) + capstonesLeft(player)

  // Static Information
  def minionsPerPlayer: Int
  def capstonesPerPlayer: Int

}
