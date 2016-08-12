package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.Action
import simulator.interfaces.game_elements.ActionKind.ActionKind

object GameLogic {

  /**
    * Decides whether a given State is terminal. Returns the winning player, if there is one.
    * @param state for which this method decides whether it is terminal.
    * @return The winning player, if any.
    */
  def gameOver(state: GameState) : Option[PlayerColor] = None

  /**
    * Collects the Actions a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def availableActions(state: GameState)(implicit color: PlayerColor): List[Action] = Nil

  /**
    * Collects the Actions of a given Kind a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param actionKind the kind of Actions which are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def availableActions(state: GameState, actionKind: ActionKind)(implicit color: PlayerColor): List[Action] = Nil

  /**
    * Decides whether a given Action is applicable in a given State for a given Player.
    * @param state in which the action might be applicable
    * @param action which is checked for being applicable.
    * @param color of the player.
    * @return
    */
  def isValid(state: GameState, action: Action)(implicit color: PlayerColor): Boolean = false

}
