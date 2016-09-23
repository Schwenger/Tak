package parsing.action

import parsing.GameToken.{Capstone, Minion, Stack, Wall}
import parsing._
import simulator.interfaces.game_elements.{Move, PlaceCapstone, PlaceMinion, PlaceWall, Action => GameAction, Position => GamePosition, Slide => SlideAction}
import simulator.interfaces.game_elements.{Direction => GameDirection}

import scala.util.Try

object Compiler {

  def apply(ast: AST): Try[GameAction] = ast match {
    case Action(a) => Compiler(a)
    case Surrender => ???
    case Place(origin, kind) =>
      val src = compileOrigin(origin)
      val res = Try(compilePlaceKind(kind))
      res.map(supplier => supplier(src))
    case MoveDir(origin, dir) => Try(Move(compileOrigin(origin), dir.dir))
    case MovePos(origin, dest) => Try(Move(compileOrigin(origin), pos2dir(origin, dest)))
    case Slide(origin, dir, moves) => Try(SlideAction(compileOrigin(origin), compileMoves(moves), dir.dir))
  }

  def compileMoves(moves: MoveList): List[Int] = (moves.stones map (_.value)).toList

  def pos2dir(src: Position, dest: Position): GameDirection.Direction = {
    val deltaX = src.x.value - dest.x.value
    val deltaY = src.y.value - dest.y.value
    (deltaX, deltaY) match {
      case (1, 0)  => GameDirection.Left
      case (-1, 0) => GameDirection.Right
      case (0, 1)  => GameDirection.Down
      case (0, -1) => GameDirection.Up
    }
  }

  def compilePlaceKind(t: TokenKind): GamePosition => GameAction = t.kind match {
    case Capstone => PlaceCapstone
    case Minion   => PlaceMinion(_, pseudo = false)
    case Wall     => PlaceWall
    case Stack    => throw ActionCompilerError("Cannot place Stacks!")
  }

  def compileOrigin(origin: Position): GamePosition = GamePosition(origin.x.value, origin.y.value)

}
