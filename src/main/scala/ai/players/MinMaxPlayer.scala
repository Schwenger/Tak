package ai.players

import ai.evaluation.Evaluator
import ai.search.MinMax
import simulator.{GameState, Player, PlayerMapping}
import simulator.elements.{Action, Position, Result}
import simulator.PlayerColor.PlayerColor
import simulator.logic.ActionSupplier

class MinMaxPlayer(override val kind: PlayerColor, eval: Evaluator, depth: Int = 3, override val boardSize: Int) extends Player {
  val supplier: (GameState, PlayerColor) => Seq[Action] = (state, color) => ActionSupplier(state)(color)

  /**
    * Called once before the actual game starts.
    *
    * @param state initial state including all game parameters like board size.
    */
  override def init(state: GameState): Unit = ()

  /**
    * Called multiple times during one game.
    * Computes the next action the player deems most sensible by their own metrics.
    *
    * @param turn  this player's and the opponent's last action
    * @param state the current game state
    * @return the action that is supposed to be executed next for this player.
    */
override def nextAction(turn: PlayerMapping[Action], state: GameState): Action = {
  MinMax(state, eval, supplier, depth, kind)
}

  /**
    * Called one after init and before the first call to nextAction.
    * Computes the position where the opponents first Minion should be placed. This corresponds to the first action.
    *
    * @param state either an empty board or a board with a token of this player is already placed
    * @return the position for the opponents first Minion
    */
  override def firstAction(state: GameState): Position = {
    // We compute the first turn from the other player's perspective
    val action = MinMax(state, eval, supplier, depth, !kind)
    action.origin
  }

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    *
    * @param result information about the winner and the final state.
    */
  override def accept(result: Result): Unit = ()
}
