package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.Action
import simulator.interfaces.game_elements.ActionKind.ActionKind

object GameLogic {

  def gameOver(state: State) : Boolean = false

  def availableActions(state: State)(implicit color: PlayerColor): List[Action] = Nil

  def availableActions(state: State, actionKind: ActionKind)(implicit color: PlayerColor): List[Action] = Nil

  def isValid(action: Action)(implicit color: PlayerColor): Boolean = false

}
