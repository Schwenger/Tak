package ai.evaluation

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

trait Evaluator {

  val color: PlayerColor

  def apply(state: GameState): Int

}
