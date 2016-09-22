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

  def number: Parser[NUMBER] = "[0-9]".r ^^ { s => NUMBER(s.toInt) }

  def surrender = "surrender|giveup|forfeit".r ^^ (_ => SURRENDER)
  def placecmd  = "place|set".r ^^ (_ => PLACECMD)
  def position  = "position" ^^ (_ => POSITION)
  def capstone  = "capstone" ^^ (_ => CAPSTONE)
  def minion    = "minion"   ^^ (_ => MINION)
  def north     = "north"    ^^ (_ => NORTH)
  def south     = "south"    ^^ (_ => SOUTH)
  def wall      = "wall"     ^^ (_ => WALL)
  def movecmd   = "move"     ^^ (_ => MOVECMD)
  def take      = "take"     ^^ (_ => TAKE)
  def drop      = "drop"     ^^ (_ => DROP)
  def from      = "from"     ^^ (_ => FROM)
  def west      = "west"     ^^ (_ => WEST)
  def east      = "east"     ^^ (_ => EAST)
  def at        = "at"       ^^ (_ => AT)
  def to        = "to"       ^^ (_ => TO)
  def lpar      = "("        ^^ (_ => LPAR)
  def rpar      = ")"        ^^ (_ => RPAR)
  def comma     = ","        ^^ (_ => COMMA)

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      number | surrender | placecmd | movecmd | position | capstone | minion | north | south | wall | take | drop |
      from | west | east | at | to | lpar | rpar | comma
    ))
  }

}
