package ai.evaluation

import ai.evaluation.Classification.Classification
import simulator.PlayerColor.PlayerColor

class Interpolator(evaluators: WeightEvalMap, classifiers: List[AbstractClassifier]) extends AbstractEvaluator {

  override def apply(color: PlayerColor) = {
    val eval: Map[Evaluator, Double] = evaluators map { tup => (tup._1(color), tup._2) }
        val classi: List[Classifier] = classifiers map (_(color))
    state => {
      val f = (eval map { case (ev, w) => w * ev(state) }).sum
      val g: Classification = classi.foldLeft(Classification.neutral)((accu, classifier) => accu * classifier(state))
      g * f
    }
  }

}

/* This class is convenience only. It's not easy to maintain but easy to use for a quick result. */
class DefaultInterpolator(domination: Double = 0, minEnemyDomination: Double = 0, streetLength: Double = 0)
  extends Interpolator(Map(
    Domination -> domination,
    MinEnemyDomination -> minEnemyDomination,
    StreetLength -> streetLength
  ) filter { case (ev, w) => w != 0 }, List(GoalIndicator))

object DefaultInterpolator {

  def respective(strength: Int) = strength match {
    case 1 => new DefaultInterpolator()
    case 2 => allEqual
    case 3 => streetLengthDom
    case 4 => domStreetLength1
    case _ => domStreetLength2
  }

  def allEqual: AbstractEvaluator = new DefaultInterpolator(1, 1, 1)

  def streetLength = new DefaultInterpolator(streetLength = 1)

  def streetLengthDom = new DefaultInterpolator(streetLength = 10, minEnemyDomination = 5, domination = 1)

  def domStreetLength1 = new DefaultInterpolator(streetLength = 5, minEnemyDomination = 20, domination = 1)

  def domStreetLength2 = new DefaultInterpolator(streetLength = 5, minEnemyDomination = 10, domination = 1)

}
