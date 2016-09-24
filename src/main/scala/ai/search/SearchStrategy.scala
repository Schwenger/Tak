package ai.search

import ai.evaluation.Evaluator
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Action

trait SearchStrategy {

  def apply(state: GameState, eval: Evaluator, actionSupplier: (GameState, PlayerColor) => Seq[Action], depth: Int,
            color: PlayerColor): Action

}
