package simulator.interfaces.game_elements

case class Position(x: Int, y: Int)

// (0,0) is bottom right
object Direction {
  sealed trait Direction
  case object Right extends Direction {
    def apply(pos: Position) = Position(pos.x + 1, pos.y)
  }
  case object Left extends Direction {
    def apply(pos: Position) = Position(pos.x - 1, pos.y)
  }
  case object Up extends Direction {
    def apply(pos: Position) = Position(pos.x + 1, pos.y)
  }
  case object Down extends Direction {
    def apply(pos: Position) = Position(pos.x - 1, pos.y)
  }
}
