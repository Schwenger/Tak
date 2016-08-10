package simulator.interfaces.game_elements

import simulator.interfaces.game_elements.Direction.Direction

object ActionKind {
  sealed trait ActionKind
  case object Move extends ActionKind
  case object Slide extends ActionKind
  case object Place extends ActionKind
}

sealed trait Action

case class Slide(origin: Position, stones: List[Int], dir: Direction) extends Action
case class Move(origin: Position, dir: Direction) extends Action
case class PlaceToken(origin: Position) extends Action
case class PlaceWall(origin: Position) extends Action
case class PlaceCapstone(origin: Position) extends Action


