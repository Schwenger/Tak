package ai.evaluation
import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

object TokenCount extends Evaluator {

  override def apply(color: PlayerColor, state: GameState): Int = state.domination(color)

}
