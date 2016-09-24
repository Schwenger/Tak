package simulator.interfaces.elements

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

case class Result(winner: PlayerColor, state: GameState)
