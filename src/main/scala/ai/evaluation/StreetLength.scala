package ai.evaluation

import simulator.PlayerColor.PlayerColor
import simulator.analysis.Analyzer

object StreetLength extends AbstractEvaluator {

  override def apply(color: PlayerColor): Evaluator = state => {
    val hori = Analyzer.minStreetDist(state, hor = true)
    val vert = Analyzer.minStreetDist(state, hor = false)
    state.size - List(hori(color), vert(color)).min
}

}
