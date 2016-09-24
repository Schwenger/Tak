package parsing.action

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Reader, Position => InputPos}
import simulator.interfaces.game_elements.{Direction => GameDirection}

import scala.util.Try

object Parser extends Parsers {

  override type Elem = ActionParseToken

  def apply(tokens: Seq[ActionParseToken]): Try[AST] = {
    val reader = new TokenReader(tokens)
    action(reader) match {
      case NoSuccess(msg, next) => scala.util.Failure(ActionParserError(msg))
      case Success(result, next) => scala.util.Success(result)
    }
  }

  private def action: Parser[ASTAction] =
    (move | slide | place | surrender) ^^ { case (a: AST) => ASTAction(a) }


  private def number: Parser[ASTNumber] = accept("number", { case value @ NUMBER(v) => ASTNumber(v.toInt) })

  private def token: Parser[ASTTokenKind] =
    accept("number", {
      case id @ CAPSTONE => ASTTokenKind(GameToken.Capstone)
      case id @ MINION => ASTTokenKind(GameToken.Minion)
      case id @ WALL => ASTTokenKind(GameToken.Wall)
      case id @ STACK => ASTTokenKind(GameToken.Stack)
    })

  private def position: Parser[ASTPosition] = {
    (
      opt(POSITION) ~ LPAR ~> number ~ (COMMA ~> number) <~ RPAR
        | opt(POSITION) ~> number ~ (COMMA ~> number)
      ) ^^ { case x ~ y => ASTPosition(x, y) }
  }

  private def surrender: Parser[ASTSurrender.type] =
    SURRENDER ^^ (_ => ASTSurrender)


  private def movelist: Parser[ASTMoveList] =
    rep1sep(number, COMMA) ^^ ASTMoveList

  private def direction: Parser[ASTDirection] =
    (NORTH | SOUTH | EAST | WEST) ^^ {
      case NORTH => ASTDirection(GameDirection.Up)
      case SOUTH => ASTDirection(GameDirection.Down)
      case EAST => ASTDirection(GameDirection.Right)
      case WEST => ASTDirection(GameDirection.Left)
      case _ => throw new IllegalArgumentException("How can this happen?!")
    }

  private def slide: Parser[ASTSlide] =
    (SLIDECMD ~ opt((MY | A) ~ opt(token)) ~ opt(AT | FROM) ~> position ~ direction ~ (DROP ~> movelist)) ^^ {
      case pos ~ dir ~ moves => ASTSlide(pos, dir, moves)
    }

  private def move: Parser[ASTMove] =
    (MOVECMD ~ opt((MY | A) ~ opt(token)) ~ opt(AT | FROM) ~> position ~ (opt(TO) ~> (position | direction))) ^^ {
      case src ~ (dest: ASTPosition) => ASTMovePos(src, dest)
      case src ~ (dir: ASTDirection) => ASTMoveDir(src, dir)
    }

  private def place: Parser[ASTPlace] =
    (PLACECMD ~ opt(A | MY) ~> token ~ (opt(AT) ~> position)) ^^ {
      case kind ~ pos => ASTPlace(pos, kind)
    }

  class TokenReader(tokens: Seq[ActionParseToken]) extends Reader[ActionParseToken] {
    override def first: ActionParseToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: InputPos = NoPosition
    override def rest: Reader[ActionParseToken] = new TokenReader(tokens.tail)
  }

}
