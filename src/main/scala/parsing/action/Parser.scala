package parsing.action

import parsing.{AST, Action, Direction, GameToken, Move, MoveDir, MoveList, MovePos, Number, Place, Position, Slide, TokenKind}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Reader, Position => InputPos}
import simulator.interfaces.game_elements.{Direction => GameDirection}

import scala.util.Try

object Parser extends Parsers {

  override type Elem = Token

  def apply(tokens: Seq[Token]): Try[AST] = {
    val reader = new TokenReader(tokens)
    action(reader) match {
      case NoSuccess(msg, next) => scala.util.Failure(ParseException(msg))
      case Success(result, next) => scala.util.Success(result)
    }
  }

  private def action: Parser[Action] =
    (move | slide | place | SURRENDER) ^^ { case (a: AST) => Action(a) }

  private def number: Parser[Number] = accept("number", { case value @ NUMBER(v) => Number(v.toInt) })

  private def token: Parser[TokenKind] =
    accept("number", {
      case id @ CAPSTONE => TokenKind(GameToken.Capstone)
      case id @ MINION => TokenKind(GameToken.Minion)
      case id @ WALL => TokenKind(GameToken.Wall)
      case id @ STACK => TokenKind(GameToken.Stack)
    })

  private def position: Parser[Position] = {
    (
      opt(POSITION) ~ LPAR ~> number ~ (COMMA ~> number) <~ RPAR
        | opt(POSITION) ~> number ~ (COMMA ~> number)
      ) ^^ { case x ~ y => Position(x, y) }
  }

  private def movelist: Parser[MoveList] =
    rep1sep(number, COMMA) ^^ MoveList

  private def direction: Parser[Direction] =
    (NORTH | SOUTH | EAST | WEST) ^^ {
      case NORTH => Direction(GameDirection.Up)
      case SOUTH => Direction(GameDirection.Down)
      case EAST => Direction(GameDirection.Right)
      case WEST => Direction(GameDirection.Left)
    }

  private def slide: Parser[Slide] =
    (SLIDECMD ~> position ~ direction ~ ((DROP | TAKE) ~> movelist)) ^^ {
      case pos ~ dir ~ moves => Slide(pos, dir, moves)
    }

  private def move: Parser[Move] =
    (MOVECMD ~> position ~ (opt(TO) ~> (position | direction))) ^^ {
      case src ~ (dest: Position) => MovePos(src, dest)
      case src ~ (dir: Direction) => MoveDir(src, dir)
    }

  private def place: Parser[Place] =
    (PLACECMD ~ opt(A) ~> token ~ (opt(AT) ~> position)) ^^ {
      case kind ~ pos => Place(pos, kind)
    }

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: InputPos = NoPosition
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

}
