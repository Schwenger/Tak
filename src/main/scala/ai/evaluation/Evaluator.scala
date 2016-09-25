package ai.evaluation

import simulator.PlayerColor.PlayerColor

trait Evaluator {

  def apply(color: PlayerColor): Eval

}


