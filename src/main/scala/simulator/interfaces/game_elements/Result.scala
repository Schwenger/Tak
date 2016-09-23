package simulator.interfaces.game_elements

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor

case class Result(winner: PlayerColor, state: GameState)
