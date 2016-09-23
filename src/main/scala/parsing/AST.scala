package parsing

import simulator.interfaces.game_elements.Direction.{Direction => GameDirection}

trait AST

case class Action(a: AST) extends AST

case object Surrender extends AST
case class Place(origin: Position, kind: TokenKind) extends AST
sealed trait Move extends AST
case class MoveDir(origin: Position, dir: Direction) extends Move
case class MovePos(origin: Position, pos: Position) extends Move
case class Slide(origin: Position, dir: Direction, moves: MoveList) extends AST
case class MoveList(stones: Seq[Number]) extends AST
case class Direction(dir: GameDirection) extends AST
case class TokenKind(kind: GameToken.Kind) extends AST
case class Position(x: Number, y: Number) extends AST
case class Number(value: Int) extends AST

object GameToken {
  sealed trait Kind
  case object Capstone extends Kind
  case object Minion extends Kind
  case object Wall extends Kind
  case object Stack extends Kind
}
object GameTokenKind extends Enumeration {
  val Capstone, Minion, Wall, Stack = Value
}