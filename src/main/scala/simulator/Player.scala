package simulator

import simulator.PlayerColor.{Black, PlayerColor, Red}
import simulator.elements.{Action, Position, Result}

object PlayerColor {
  sealed trait PlayerColor {
    def unary_!() = this match {
      case Red => Black
      case Black => Red
    }
  }
  case object Red extends PlayerColor
  case object Black extends PlayerColor
}

case class PlayerMapping[T](red: T, black: T) {
  def apply(color: PlayerColor): T = color match {
    case Red => red
    case Black => black
  }
  def map[S](trans: (T, T) => (S, S)): PlayerMapping[S] =
    (PlayerMapping[S] _).tupled(trans(this.red, this.black))
  def transform[S](trans: (PlayerColor, T) => S): PlayerMapping[S] =
    PlayerMapping(trans(Red, this.red), trans(Black, this.black))
}

trait Player {

  implicit val kind: PlayerColor
  val boardSize: Int

  /**
    * Called once before the actual game starts.
    * @param state initial state including all game parameters like board size.
    */
  def init(state: GameState)

  /**
    * Called multiple times during one game.
    * Computes the next action the player deems most sensible by their own metrics.
    * @param turn this player's and the opponent's last action
    * @param state the current game state
    * @return the action that is supposed to be executed next for this player.
    */
  def nextAction(turn: PlayerMapping[Action], state: GameState): Action

  /**
    * Called once after init and before the first call to nextAction.
    * Computes the position where the opponents first Minion should be placed. This corresponds to the first action.
    * @param state either an empty board or a board with a token of this player is already placed
    * @return the position for the opponents first Minion
    */
  def firstAction(state: GameState): Position

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    * @param result information about the winner and the final state.
    */
  def accept(result: Result)

}
