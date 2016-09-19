package simulator.interfaces

import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}
import simulator.interfaces.game_elements.{Action, Position, Result}

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
  def map[S](trans: (T, PlayerColor) => S): PlayerMapping[S] =
    PlayerMapping(trans(this.red, Red), trans(this.black, Black))
}

trait Player {

  val kind: PlayerColor

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
    * @param occupied reports the only already occupied game position if the player is black
    * @return the position for the opponents first Minion
    */
  def firstAction(occupied: Option[Position]): Position

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    * @param result information about the winner and the final state.
    */
  def accept(result: Result)

}
