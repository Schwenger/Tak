package parsing.action

trait ActionInputError extends Throwable
case class ActionLexerError(msg: String) extends ActionInputError
case class ActionParserError(msg: String) extends ActionInputError
case class ActionCompilerError(msg: String) extends ActionInputError
