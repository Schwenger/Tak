package simulator.model

import simulator.interfaces.game_elements.{Position, Token}

case class Field(pos: Position) {
  var content: Option[Token] = None
}
