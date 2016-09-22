package ai.players

import ai.evaluation.Evaluator
import ai.search.MinMax
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Action, Minion, Position, Result}
import simulator.interfaces.{GameLogic, GameState, Player, PlayerMapping}

class MinMaxPlayer(color: PlayerColor, eval: Evaluator, depth: Int = 3, boardSize: Int) extends Player {
  override val kind: PlayerColor = color
  val supplier: GameState => Seq[Action] = GameLogic.availableActions(_)(color)

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
  MinMax(state, eval, supplier, depth, color)
}

  /**
    * Called one after init and before the first call to nextAction.
    * Computes the position where the opponents first Minion should be placed. This corresponds to the first action.
    *
    * @param occupied reports the only already occupied game position if the player is black
    * @return the position for the opponents first Minion
    */
  override def firstAction(occupied: Option[Position]): Position = {
    // We compute the first turn from the other player's perspective
    val board = new GameState(boardSize)
    occupied.foreach(p => board.setField(p, Minion(color)))
    val action = MinMax(board, eval, supplier, depth, !color)
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
