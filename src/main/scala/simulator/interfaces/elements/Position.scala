package simulator.interfaces.elements

case class Position(x: Int, y: Int)

// (0,0) is bottom right
object Direction {
  val directions = Seq(Right, Left, Up, Down)
  sealed trait Direction {
    def apply(pos: Position): Position

    /**
      * Provides the opposite direction
      * @return the opposite direction
      */
    def unary_!(): Direction

    /**
      * Provides the orthogonal directions to the given one.
      * @return a pair of orthogonal directions without any guarantee for the order
      */
    def orth: (Direction, Direction) = this match {
      case Right | Left => (Up, Down)
      case Up | Down => (Left, Right)
    }
  }
  case object Right extends Direction {
    def apply(pos: Position) = Position(pos.x + 1, pos.y)
    override def unary_!(): Direction = Left
  }
  case object Left extends Direction {
    def apply(pos: Position) = Position(pos.x - 1, pos.y)
    override def unary_!(): Direction = Right
  }
  case object Up extends Direction {
    def apply(pos: Position) = Position(pos.x, pos.y + 1)
    override def unary_!(): Direction = Down
  }
  case object Down extends Direction {
    def apply(pos: Position) = Position(pos.x, pos.y - 1)
    override def unary_!(): Direction = Up
  }
}
