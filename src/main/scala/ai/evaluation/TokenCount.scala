package ai.evaluation
import simulator.analysis.Analyzer
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

object TokenCount extends Evaluator {

  override def apply(color: PlayerColor, state: GameState): Int = Analyzer.domination(state)(color)

}
