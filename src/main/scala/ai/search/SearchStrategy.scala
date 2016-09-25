package ai.search

import ai.evaluation.Eval
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Action

trait SearchStrategy {

  def apply(state: GameState, eval: Eval, actionSupplier: (GameState, PlayerColor) => Seq[Action], depth: Int,
            color: PlayerColor): Action

}
