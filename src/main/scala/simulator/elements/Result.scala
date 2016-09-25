package simulator.elements

import simulator.GameState
import simulator.PlayerColor.PlayerColor

case class Result(winner: PlayerColor, state: GameState)
