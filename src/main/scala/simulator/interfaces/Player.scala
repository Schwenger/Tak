package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Action, Position, Result, Turn}

object PlayerColor {
  sealed trait PlayerColor
  case object Red extends PlayerColor
  case object Black extends PlayerColor
}

trait Player {

  val kind: PlayerColor

  /**
    * Called once before the actual game starts.
    * @param state initial state including all game parameters like board size.
    */
  def init(state: State)

  /**
    * Called multiple times during one game.
    * Computes the next action the player deems most sensible by their own metrics.
    * @param opponent the opponents last action
    * @param player this players last action
    * @param state the current game state
    * @return the action that is supposed to be executed next for this player.
    */
  def nextAction(turn: Turn, state: State): Action

  /**
    * Called one after init and before the first call to nextAction.
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
