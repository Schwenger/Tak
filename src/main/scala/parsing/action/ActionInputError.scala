package parsing.action

trait ActionInputError
case class ActionLexerError(msg: String) extends ActionInputError
case class ActionParserError(msg: String) extends ActionInputError
