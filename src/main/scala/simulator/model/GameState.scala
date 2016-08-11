package simulator.model

import simulator.interfaces.PlayerColor.{PlayerColor, Red}
import simulator.interfaces.game_elements.{Field, Position}
import simulator.interfaces.{PlayerMapping, State}

import scala.collection.mutable

class GameState(override val size: Int) extends State {
  // Static Data
  val numTokens = Map(4 -> (15, 0), 5 -> (20, 1), 6 -> (30, 1), 7 -> (40, 2), 8 -> (50, 2))

  // Dynamic Data
  private val board = mutable.Seq.tabulate(size, size)((x, y) => Field(Position(x, y)))
  private var tokens: PlayerMapping[(Int, Int)] = PlayerMapping(numTokens(size), numTokens(size))

  // Board Information
  override def apply(pos: Position): Field = this(pos.x, pos.y)

  override def apply(x: Int, y: Int): Field = board(x)(y)

  // Analytical Information (caller convenience)
  override def freeFields: Int = ???

  override def domination: PlayerMapping[Int] = ???

  override def longestStreet: PlayerMapping[Int] = ???

  // Player Information
  override def minionsLeft(player: PlayerColor): Int = tokens(player)._1

  override def capstonesLeft(player: PlayerColor): Int = tokens(player)._2

  // Internal use only, i.e. not inherited from State

  def checksum = 13

  def removeToken(player: PlayerColor, minion: Boolean) = {
    tokens = tokens.map((old, color) => (color == player, minion) match {
      case (true, true)  => (old._1 - 1, old._2)
      case (true, false) => (old._1, old._2 - 1)
      case _ => old
    })
  }


  // Static Information


  override def minionsPerPlayer: Int = numTokens(size)._1

  override def capstonesPerPlayer: Int = numTokens(size)._2

}
