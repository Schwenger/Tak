package ai.search

import ai.evaluation.Evaluator
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Action

class SearchNode[S](state: GameState, action: Action, context: S)

trait SearchStrategy {

  def apply(state: GameState, eval: Evaluator, actionSupplier: GameState => Seq[Action], depth: Int, color: PlayerColor): Action

}
