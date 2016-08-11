package simulator.interfaces.game_elements

import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}

case class Turn(red: Action, black: Action){
  def apply(color: PlayerColor): Action = color match {
    case Red => red
    case Black => black
  }
}
