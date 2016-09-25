package ai.search

import ai.evaluation.Eval
import simulator.elements.Action
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

trait SearchStrategy {

  def apply(state: GameState, eval: Eval, actionSupplier: (GameState, PlayerColor) => Seq[Action], depth: Int,
            color: PlayerColor): Action

}
