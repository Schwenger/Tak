import org.scalatest.{FlatSpec, Matchers}
import parsing.action.GameToken.{Capstone, Minion, Wall}
import parsing._
import parsing.action._
import simulator.interfaces.game_elements.Direction

import scala.util.{Failure, Success, Try}

class ActionParserSpec extends FlatSpec with Matchers {

  // Note: This is only very shallow testing here.
  // It would be nice to have better tests but it's not a priority and parsers are hard to test extensively.
  // And with 'hard' I mean a lot of tedious work. I'd need a HiWi for that.

  def fixture =
    new {
      def check(parseResult: Try[AST], eval: AST => Unit) = parseResult match {
        case Failure(e) => throw e
        case Success(action) => eval(action)
      }
      def parse(cmd: String): Try[AST] = Lexer(cmd).flatMap(Parser(_))
    }

  "ActionParser" should "parse a place for a Minion correctly" in {
    val f = fixture
    def test(ast: AST): Unit = {
      ast shouldBe a [ASTAction]
      val action = ast.asInstanceOf[ASTAction]
      action.a shouldBe a [ASTPlace]
      val place = action.a.asInstanceOf[ASTPlace]
      place.origin should be (ASTPosition(ASTNumber(1), ASTNumber(3)))
      place.kind should be (ASTTokenKind(Minion))
    }
    val parseResult = f.parse("place minion (1,3)")
    f.check(parseResult, test)
  }

  it should "parse a place for a Wall correctly" in {
    val f = fixture
    val p = f.parse("place wall (1,3)")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTPlace(ASTPosition(ASTNumber(1), ASTNumber(3)), ASTTokenKind(Wall))))
    }
    f.check(p, test)
  }

  it should "parse a slide correctly" in {
    val f = fixture
    val p = f.parse("slide (3,1) north drop 7, 5")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTSlide(ASTPosition(ASTNumber(3), ASTNumber(1)), ASTDirection(Direction.Up), ASTMoveList(Seq(ASTNumber(7), ASTNumber(5))))))
    }
    f.check(p, test)
  }

  it should "parse a movedir correctly" in {
    val f = fixture
    val p = f.parse("move my (3,1) north")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTMoveDir(ASTPosition(ASTNumber(3), ASTNumber(1)), ASTDirection(Direction.Up))))
    }
    f.check(p, test)
  }

  it should "parse a movepos correctly" in {
    val f = fixture
    val p = f.parse("move my (3,1) to (3,2)")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTMoveDir(ASTPosition(ASTNumber(3), ASTNumber(1)), ASTDirection(Direction.Up))))
    }
    f.check(p, test)
  }

  it should "parse a place for a Capstone correctly" in {
    val f = fixture
    val p = f.parse("place capstone (1,3)")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTPlace(ASTPosition(ASTNumber(1), ASTNumber(3)), ASTTokenKind(Capstone))))
    }
    f.check(p, test)
  }

  it should "allow additional worlds in place instructions" in {
    val f = fixture
    val p = f.parse("place a minion at (1,3)")
    def test(ast: AST): Unit = {
      ast should be (ASTAction(ASTPlace(ASTPosition(ASTNumber(1), ASTNumber(3)), ASTTokenKind(Minion))))
    }
    f.check(p, test)
  }

  it should "detect invalid token kinds" in {
    val f = fixture
    val p = f.parse("place a woggle at (1,3)")
    p shouldBe 'isFailure
  }

  it should "parse a surrender" in {
    val f = fixture
    val p = f.parse("surrender")
    def test(ast: AST): Unit = {
      ast should be (ASTSurrender)
    }
  }

  it should "allow different ways of notation for a position" in {
    val cmds = Seq(
      "place a minion at Position(1,3)",
      "place a minion at (1,3)",
      "place a minion at (1, 3)",
      "place a minion at (1    , 3)",
      "place a minion at (  1    , 3  )",
      "place a minion at Position(  1    , 3  )"
    )
    val f = fixture
    for { cmd <- cmds } {
      val p = f.parse(cmd)
      def test(ast: AST): Unit = {
        ast should be (ASTAction(ASTPlace(ASTPosition(ASTNumber(1), ASTNumber(3)), ASTTokenKind(Minion))))
      }
      f.check(p, test)
    }
  }
}



