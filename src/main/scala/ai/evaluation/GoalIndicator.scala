package ai.evaluation

import simulator.PlayerColor.PlayerColor
import simulator.logic.GameOver

object GoalIndicator extends Evaluator {

  override def apply(color: PlayerColor): Eval = state =>
    GameOver(state) match {
      case Some(c) if c == color => println("game over"); Double.PositiveInfinity
      case Some(c) => 0
      case _ => 1
    }
}
