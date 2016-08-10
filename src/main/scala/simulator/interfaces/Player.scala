package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Action, Result}

object PlayerColor {
  sealed trait PlayerColor
  case object Red extends PlayerColor
  case object Black extends PlayerColor
}

trait Player {

  val kind: PlayerColor

  def init(state: State)

  def nextAction(opponent: Action, player: Action, state: State): Action

  def accept(result: Result)

}
