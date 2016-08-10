package simulator.logic

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.{GameLogic, State}
import simulator.interfaces.game_elements.Action

import scala.util.{Failure, Success, Try}

object ActionExecutor {
  /**
    * Executes a given action in the given state.
    * @param action to be executed
    * @param state in which the action is executed
    * @param player who takes the action
    * @return the state after the execution
    */
  // NOTE: The action executor always copies the whole board which is extremely inefficient.
  // TODO: when handing a state to the player, record checksum or similar to avoid spurious behavior
  def apply(action: Action, state: State, player: PlayerColor): State = {
    assert(GameLogic.isValid(state, action)(player))
    null // TODO
  }
}
