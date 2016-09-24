package ai.evaluation
import simulator.analysis.Analyzer
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

class TokenCount(override val color: PlayerColor) extends Evaluator {

  override def apply(state: GameState): Int = Analyzer.domination(state)(color)

}
