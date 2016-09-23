import org.scalatest.{FlatSpec, Matchers}
import parsing.action._

class ActionLexerSpec extends FlatSpec with Matchers {

  "ActionLexer" should "lex correctly" in {
    val input = "move place at from 3 position )"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (7)
    tokens(0) shouldBe MOVECMD
    tokens(1) shouldBe PLACECMD
    tokens(2) shouldBe AT
    tokens(3) shouldBe FROM
    tokens(4) shouldBe NUMBER("3")
    tokens(5) shouldBe POSITION
    tokens(6) shouldBe RPAR
  }

  it should "lex numbers" in {
    val input = "3"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (1)
    tokens.head shouldBe NUMBER("3")
  }

  it should "lex number sequences" in {
    val input = "3, 3"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (3)
    tokens.head shouldBe NUMBER("3")
    tokens(1) shouldBe COMMA
    tokens(2) shouldBe NUMBER("3")
  }

  it should "lex slides" in {
    val input = "slide (3,1) north drop 7,5"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (11)
    tokens(0) shouldBe SLIDECMD
    tokens(1) shouldBe LPAR
    tokens(2) shouldBe NUMBER("3")
    tokens(3) shouldBe COMMA
    tokens(4) shouldBe NUMBER("1")
    tokens(5) shouldBe RPAR
    tokens(6) shouldBe NORTH
    tokens(7) shouldBe DROP
    tokens(8) shouldBe NUMBER("7")
    tokens(9) shouldBe COMMA
    tokens(10) shouldBe NUMBER("5")
  }

  it should "lex surrenders" in {
    val input = "surrender"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (1)
    tokens.head shouldBe SURRENDER
  }

  it should "lex parenthesis" in {
    val input = "("
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (1)
    tokens.head shouldBe LPAR
  }

  it should "lex at" in {
    val input = "at"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (1)
    tokens.head shouldBe AT
  }

  it should "lex more than one item" in {
    val input = "at to"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (2)
    tokens.head shouldBe AT
    tokens(1) shouldBe TO
  }

  it should "ignore cases" in {
    val input = "at PoSiTion tO"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (3)
    tokens.head shouldBe AT
    tokens(1) shouldBe POSITION
    tokens(2) shouldBe TO
  }

  it should "allow a mix of numbers and keywords" in {
    val input = "at 3 to"
    val lexRes = Lexer(input)
    lexRes shouldBe 'isSuccess
    val tokens: Seq[ActionParseToken] = lexRes.get
    tokens.length should be (3)
    tokens.head shouldBe AT
    tokens(1) shouldBe NUMBER("3")
    tokens(2) shouldBe TO
  }
}

