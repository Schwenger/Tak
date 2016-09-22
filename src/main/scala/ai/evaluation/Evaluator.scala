package ai.evaluation

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

trait Evaluator {

  def apply(color: PlayerColor, state: GameState): Int

}
