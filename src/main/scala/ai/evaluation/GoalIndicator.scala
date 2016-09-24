package ai.evaluation

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.logic.GameOver

class GoalIndicator(override val color: PlayerColor) extends Evaluator {

  override def apply(state: GameState): Int = GameOver(state) match {
    case Some(c) if c == color => 2
    case Some(c) if c != color => 0
    case None => 1
  }
}
