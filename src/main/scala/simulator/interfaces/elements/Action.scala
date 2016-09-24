package simulator.interfaces.elements

import simulator.interfaces.elements.Direction.Direction

object ActionKind {
  sealed trait ActionKind
  case object Move extends ActionKind
  case object Slide extends ActionKind
  case object Place extends ActionKind
  case object Surrender extends ActionKind
}

sealed trait Action {
  val origin: Position
}

case class Slide(origin: Position, stones: List[Int], dir: Direction) extends Action
case class Move(origin: Position, dir: Direction) extends Action
case class PlaceMinion(origin: Position) extends Action
case class PlaceWall(origin: Position) extends Action
case class PlaceCapstone(origin: Position) extends Action
case class Surrender(origin: Position = Position(0,0)) extends Action
