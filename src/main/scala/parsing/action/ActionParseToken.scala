package parsing.action

sealed trait ActionParseToken

case object SURRENDER extends ActionParseToken
case object PLACECMD  extends ActionParseToken
case object SLIDECMD  extends ActionParseToken
case object CAPSTONE  extends ActionParseToken
case object POSITION  extends ActionParseToken
case object MOVECMD   extends ActionParseToken
case object MINION    extends ActionParseToken
case object STACK     extends ActionParseToken
case object COMMA     extends ActionParseToken
case object NORTH     extends ActionParseToken
case object SOUTH     extends ActionParseToken
case object LPAR      extends ActionParseToken
case object RPAR      extends ActionParseToken
case object WEST      extends ActionParseToken
case object EAST      extends ActionParseToken
case object WALL      extends ActionParseToken
case object DROP      extends ActionParseToken
case object FROM      extends ActionParseToken
case object MY        extends ActionParseToken
case object AT        extends ActionParseToken
case object TO        extends ActionParseToken
case object A         extends ActionParseToken

case class NUMBER(value: String) extends ActionParseToken
