package parsing.action

import simulator.elements.Action

import scala.util.Try

object ActionParser {
  def apply(cmd: String): Try[Action] = {
    Lexer(cmd) flatMap (Parser(_)) flatMap (Compiler(_))
  }

}

