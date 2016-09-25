package simulator.analysis

import simulator.{GameState, PlayerMapping}
import simulator.PlayerColor.{Black, Red}

object Analyzer {

  def freeFields(state: GameState): Int = state.map(_.isEmpty).map(_.count(b => b)).sum

  def domination(state: GameState): PlayerMapping[Int] =
    state.fold(PlayerMapping(0,0))((accu, field) =>
      field.map(_.player) match {
        case Some(Red) => PlayerMapping(accu.red + 1, accu.black)
        case Some(Black) => PlayerMapping(accu.red, accu.black + 1)
        case None => accu
      }
    )

}
