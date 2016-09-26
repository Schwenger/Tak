package ai.evaluation
import simulator.analysis.Analyzer
import simulator.PlayerColor.PlayerColor

object Domination extends AbstractEvaluator {

  override def apply(color: PlayerColor): Evaluator = state => Analyzer.domination(state)(color)

}
