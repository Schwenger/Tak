package ai.players

import simulator.{GameState, Player, PlayerMapping}
import simulator.elements.{Action, Position, Result}
import simulator.PlayerColor.PlayerColor
import simulator.logic.ActionSupplier

import scala.util.Random

class RandomPlayer(override val kind: PlayerColor, override val boardSize: Int) extends Player {

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
    val available = ActionSupplier(state)(kind)
    available(Random.nextInt(available.length))
  }

  /**
    * Called once after init and before the first call to nextAction.
    * Computes the position where the opponents first Minion should be placed. This corresponds to the first action.
    *
    * @param state either an empty board or a board with a token of this player is already placed
    * @return the position for the opponents first Minion
    */
  override def firstAction(state: GameState): Position = {
    var pos: Position = null
    do {
      val x = Random.nextInt(boardSize)
      val y = Random.nextInt(boardSize)
      pos = Position(x,y)
    } while(state(pos).isDefined)
    assert(pos != null)
    pos
  }

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    *
    * @param result information about the winner and the final state.
    */
  override def accept(result: Result): Unit = ()
}
