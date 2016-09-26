package ai.search

import ai.evaluation.Evaluator
import simulator.GameState
import simulator.elements.Action
import simulator.PlayerColor.PlayerColor

trait SearchStrategy {

  def apply(state: GameState, eval: Evaluator, actionSupplier: (GameState, PlayerColor) => Seq[Action], depth: Int,
            color: PlayerColor): Action

}
