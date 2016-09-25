package ai.evaluation

import simulator.PlayerColor.PlayerColor

class Interpolator(weightedEvaluators: Seq[(Evaluator, Double)]) extends Evaluator {

  override def apply(color: PlayerColor): Eval = {

    val eval: Seq[(Eval, Double)] = weightedEvaluators map { tup => (tup._1(color), tup._2) }

    state =>
      (eval map {tup => tup._1(state) * tup._2 }).sum

  }

}
