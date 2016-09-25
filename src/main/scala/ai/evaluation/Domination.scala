package ai.evaluation
import simulator.analysis.Analyzer
import simulator.interfaces.PlayerColor.PlayerColor

object Domination extends Evaluator {

  override def apply(color: PlayerColor): Eval = state => Analyzer.domination(state)(color)

}
