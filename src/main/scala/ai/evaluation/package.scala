package ai

import ai.evaluation.Classification.Classification
import simulator.GameState

package object evaluation {

  type Evaluator = GameState => Double
  type Classifier = GameState => Classification
  type WeightEvalMap = Map[AbstractEvaluator, Double]

}
