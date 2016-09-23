package parsing.action

import simulator.interfaces.game_elements.Direction.{Direction => GameDirection}

trait AST

case class ASTAction(a: AST) extends AST

case object ASTSurrender extends AST
case class ASTPlace(origin: ASTPosition, kind: ASTTokenKind) extends AST
sealed trait ASTMove extends AST
case class ASTMoveDir(origin: ASTPosition, dir: ASTDirection) extends ASTMove
case class ASTMovePos(origin: ASTPosition, pos: ASTPosition) extends ASTMove
case class ASTSlide(origin: ASTPosition, dir: ASTDirection, moves: ASTMoveList) extends AST
case class ASTMoveList(stones: Seq[ASTNumber]) extends AST
case class ASTDirection(dir: GameDirection) extends AST
case class ASTTokenKind(kind: GameToken.Kind) extends AST
case class ASTPosition(x: ASTNumber, y: ASTNumber) extends AST
case class ASTNumber(value: Int) extends AST

object GameToken {
  sealed trait Kind
  case object Capstone extends Kind
  case object Minion extends Kind
  case object Wall extends Kind
  case object Stack extends Kind
}
