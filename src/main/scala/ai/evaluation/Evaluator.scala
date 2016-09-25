package ai.evaluation

import simulator.interfaces.PlayerColor.PlayerColor

trait Evaluator {

  def apply(color: PlayerColor): Eval

}


