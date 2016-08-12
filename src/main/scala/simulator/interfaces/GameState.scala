package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Position, Token}
import simulator.model.Field

import scala.collection.mutable

class GameState(val size: Int) {
  // Static Data
  val numTokens = Map(4 -> (15, 0), 5 -> (20, 1), 6 -> (30, 1), 7 -> (40, 2), 8 -> (50, 2))

  // Dynamic Data
  private val board = mutable.Seq.tabulate(size, size)((x, y) => Field(Position(x, y)))
  private var tokens: PlayerMapping[(Int, Int)] = PlayerMapping(numTokens(size), numTokens(size))

  // Board Information
  def apply(pos: Position): Option[Token] = this(pos.x, pos.y)

  def apply(x: Int, y: Int): Option[Token] = board(x)(y).content

  // Analytical Information (caller convenience)
  def freeFields: Int = ???

  def domination: PlayerMapping[Int] = ???

  def longestStreet: PlayerMapping[Int] = ???

  // Player Information
  def minionsLeft(player: PlayerColor): Int = tokens(player)._1

  def capstonesLeft(player: PlayerColor): Int = tokens(player)._2

  // Changing State
  def removeToken(player: PlayerColor, minion: Boolean) = {
    tokens = tokens.map((old, color) => (color == player, minion) match {
      case (true, true)  => (old._1 - 1, old._2)
      case (true, false) => (old._1, old._2 - 1)
      case _ => old
    })
  }

  def setField(pos: Position, token: Token): Unit = board(pos.x)(pos.y).content = Some(token)
  def setField(pos: Position, token: Option[Token]): Unit = token match {
    case None => clearField(pos)
    case Some(t) => setField(pos, t)
  }
  def clearField(pos: Position) = board(pos.x)(pos.y).content = None

  // Internal use only, i.e. not inherited from State

  def checksum = 13

  // Static Information

  def minionsPerPlayer: Int = numTokens(size)._1

  def capstonesPerPlayer: Int = numTokens(size)._2

  def copy = this // TODO!!!!!!

}
