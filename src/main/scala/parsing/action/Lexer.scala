package parsing.action

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def apply(input: String): Either[ActionInputError, List[Token]] = parse(tokens, input.toLowerCase) match {
    case NoSuccess(msg, next) => Left(ActionLexerError(msg))
    case Success(result, next) => Right(result)
  }

  def number: Parser[NUMBER] = "[0-9]".r ^^ { s => NUMBER(s) }

  def surrender = "surrender|give up|forfeit".r ^^ (_ => SURRENDER)
  def east      = "east|right".r ^^ (_ => EAST)
  def south     = "south|down".r ^^ (_ => SOUTH)
  def placecmd  = "place|set".r  ^^ (_ => PLACECMD)
  def west      = "west|left"    ^^ (_ => WEST)
  def north     = "north|up"     ^^ (_ => NORTH)
  def position  = "position"     ^^ (_ => POSITION)
  def capstone  = "capstone"     ^^ (_ => CAPSTONE)
  def minion    = "minion"       ^^ (_ => MINION)
  def stack     = "stack"        ^^ (_ => STACK)
  def slidecmd  = "slide"        ^^ (_ => SLIDECMD)
  def wall      = "wall"         ^^ (_ => WALL)
  def movecmd   = "move"         ^^ (_ => MOVECMD)
  def take      = "take"         ^^ (_ => TAKE)
  def drop      = "drop"         ^^ (_ => DROP)
  def from      = "from"         ^^ (_ => FROM)
  def at        = "at"           ^^ (_ => AT)
  def to        = "to"           ^^ (_ => TO)
  def my        = "my"           ^^ (_ => MY)
  def a         = "a"            ^^ (_ => A)
  def lpar      = "("            ^^ (_ => LPAR)
  def rpar      = ")"            ^^ (_ => RPAR)
  def comma     = ","            ^^ (_ => COMMA)

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      number | surrender | placecmd | movecmd | position | capstone | minion | north | south | wall | take | drop |
      from | west | east | at | to | lpar | rpar | comma | stack | slidecmd | a | my
    ))
  }

}
