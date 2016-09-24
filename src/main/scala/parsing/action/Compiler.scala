package parsing.action

import GameToken.{Capstone, Minion, Stack, Wall}
import simulator.interfaces.elements.{Move => MoveAction, PlaceCapstone, PlaceMinion, PlaceWall,
Action => GameAction, Direction => GameDirection, Position => GamePosition, Slide => SlideAction,
Surrender => SurrenderAction}

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Compiler {

  @tailrec def apply(ast: AST): Try[GameAction] = ast match {
    case ASTAction(a) => Compiler(a)
    case ASTSurrender => Success(SurrenderAction())
    case ASTPlace(origin, kind) =>
      val src = compileOrigin(origin)
      val res = Try(compilePlaceKind(kind))
      res.map(supplier => supplier(src))
    case ASTMoveDir(origin, dir) => Try(MoveAction(compileOrigin(origin), dir.dir))
    case ASTMovePos(origin, dest) => Try(MoveAction(compileOrigin(origin), pos2dir(origin, dest)))
    case ASTSlide(origin, dir, moves) => Try(SlideAction(compileOrigin(origin), compileMoves(moves), dir.dir))
  }

  def compileMoves(moves: ASTMoveList): List[Int] = (moves.stones map (_.value)).toList

  def pos2dir(src: ASTPosition, dest: ASTPosition): GameDirection.Direction = {
    val deltaX = src.x.value - dest.x.value
    val deltaY = src.y.value - dest.y.value
    (deltaX, deltaY) match {
      case (1, 0)  => GameDirection.Left
      case (-1, 0) => GameDirection.Right
      case (0, 1)  => GameDirection.Down
      case (0, -1) => GameDirection.Up
    }
  }

  def compilePlaceKind(t: ASTTokenKind): GamePosition => GameAction = t.kind match {
    case Capstone => PlaceCapstone
    case Minion   => PlaceMinion
    case Wall     => PlaceWall
    case Stack    => throw ActionCompilerError("Cannot place Stacks!")
  }

  def compileOrigin(origin: ASTPosition): GamePosition = GamePosition(origin.x.value, origin.y.value)

}
