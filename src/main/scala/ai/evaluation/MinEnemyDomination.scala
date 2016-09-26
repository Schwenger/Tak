package ai.evaluation

import simulator.PlayerColor.PlayerColor
import simulator.analysis.Analyzer

object MinEnemyDomination extends AbstractEvaluator {

  override def apply(color: PlayerColor): Evaluator = state =>
    (state.size * state.size) - Analyzer.domination(state)(!color)

}
