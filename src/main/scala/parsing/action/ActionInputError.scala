package parsing.action

class ActionInputError(msg: String) extends Throwable {
  override def toString = msg
}
case class ActionLexerError(msg: String) extends ActionInputError(msg)
case class ActionParserError(msg: String) extends ActionInputError(msg)
case class ActionCompilerError(msg: String) extends ActionInputError(msg)
