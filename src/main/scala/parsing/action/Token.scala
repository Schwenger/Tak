package parsing.action

sealed trait Token

case object SURRENDER extends Token
case object MOVECMD extends Token
case object PLACECMD extends Token
case object CAPSTONE extends Token
case object MINION extends Token
case object WALL extends Token
case object POSITION extends Token
case object LPAR extends Token
case object RPAR extends Token
case object COMMA extends Token
case object AT extends Token
case object TO extends Token
case object TAKE extends Token
case object DROP extends Token
case object FROM extends Token
case object NORTH extends Token
case object SOUTH extends Token
case object WEST extends Token
case object EAST extends Token

case class NUMBER(value: Int) extends Token
