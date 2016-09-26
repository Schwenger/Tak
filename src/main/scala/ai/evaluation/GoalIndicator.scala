package ai.evaluation

import ai.evaluation.Classification.{Loss, Neutral, Win}
import simulator.PlayerColor.PlayerColor
import simulator.logic.GameOver

object GoalIndicator extends AbstractClassifier {

  override def apply(color: PlayerColor): Classifier = state =>
    GameOver(state) match {
      case Some(c) if c == color => Win
      case Some(c) => Loss
      case _ => Neutral
    }

}
