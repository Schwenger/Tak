package simulator.analysis

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Action
import simulator.interfaces.elements.ActionKind.ActionKind

object ActionSupplier {

  /**
    * Collects the Actions a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def apply(state: GameState)(implicit color: PlayerColor): List[Action] = ???

  /**
    * Collects the Actions of a given Kind a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param actionKind the kind of Actions which are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def apply(state: GameState, actionKind: ActionKind)(implicit color: PlayerColor): List[Action] = ???

}
