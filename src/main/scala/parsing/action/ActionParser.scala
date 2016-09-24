package parsing.action

import simulator.interfaces.elements.{Action => GameAction}

import scala.util.Try

object ActionParser {
  def apply(cmd: String): Try[GameAction] = {
    Lexer(cmd) flatMap (Parser(_)) flatMap (Compiler(_))
  }

}

