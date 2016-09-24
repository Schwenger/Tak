package parsing.action

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def apply(input: String): Try[List[ActionParseToken]] = parse(tokens, input.toLowerCase) match {
    case NoSuccess(msg, next) => scala.util.Failure(ActionLexerError(msg))
    case Success(result, next) => scala.util.Success(result)
  }

  def number: Parser[NUMBER] = "[0-9]".r ^^ { s => NUMBER(s) }

  def surrender = "surrender|give up|forfeit".r ^^ (_ => SURRENDER)
  def east      = "east|right".r ^^ (_ => EAST)
  def south     = "south|down".r ^^ (_ => SOUTH)
  def placecmd  = "place|set".r  ^^ (_ => PLACECMD)
  def west      = "west|left".r  ^^ (_ => WEST)
  def north     = "north|up".r   ^^ (_ => NORTH)
  def my        = "my|the".r     ^^ (_ => MY)
  def position  = "position"     ^^ (_ => POSITION)
  def capstone  = "capstone"     ^^ (_ => CAPSTONE)
  def minion    = "minion"       ^^ (_ => MINION)
  def stack     = "stack"        ^^ (_ => STACK)
  def slidecmd  = "slide"        ^^ (_ => SLIDECMD)
  def wall      = "wall"         ^^ (_ => WALL)
  def movecmd   = "move"         ^^ (_ => MOVECMD)
  def drop      = "drop"         ^^ (_ => DROP)
  def from      = "from"         ^^ (_ => FROM)
  def at        = "at"           ^^ (_ => AT)
  def to        = "to"           ^^ (_ => TO)
  def a         = "a"            ^^ (_ => A)
  def lpar      = "("            ^^ (_ => LPAR)
  def rpar      = ")"            ^^ (_ => RPAR)
  def comma     = ","            ^^ (_ => COMMA)

  def tokens: Parser[List[ActionParseToken]] = {
    phrase(rep1(
      number | surrender | placecmd | movecmd | position | capstone | minion | north | south | wall | drop |
      from | west | east | at | to | lpar | rpar | comma | stack | slidecmd | my | a
    ))
  }

}
