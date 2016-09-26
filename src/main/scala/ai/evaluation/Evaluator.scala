package ai.evaluation

import simulator.PlayerColor.PlayerColor

trait AbstractEvaluator {
  def apply(color: PlayerColor): Evaluator
}

trait AbstractClassifier {
  def apply(color: PlayerColor): Classifier
}

object Classification {
  sealed abstract class Classification(val value: Double) {
    def *(that: Classification): Classification = (this, that) match {
      case (Loss, _) | (_, Loss) => Loss
      case (Win, _)  | (Win, _) => Win
      case _ => Neutral
    }
    def *(that: Double): Double = that * this.value
  }
  case object Neutral extends Classification(1)
  case object Win extends Classification(Double.PositiveInfinity)
  case object Loss extends Classification(0)

  def neutral: Classification = Neutral
}

