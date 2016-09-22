import org.scalatest.{FlatSpec, Matchers}
import parsing.action.ActionParser
import simulator.interfaces.game_elements._

import scala.util.{Failure, Success, Try}

class ActionParserSpec extends FlatSpec with Matchers {

  def fixture =
    new {
      def check(parseResult: Try[Action], eval: Action => Unit) = parseResult match {
        case Failure(e) => throw e
        case Success(action) => eval(action)
      }
    }

  "ActionParser" should "parse a place for a Minion correctly" in {
    val f = fixture
    def test(action: Action): Unit = {
      action shouldBe a [PlaceMinion]
      action.origin should be (Position(1,3))
      action.asInstanceOf[PlaceMinion] should not be 'pseudo
    }
    val parseResult = ActionParser("place minion (1,3)")
    f.check(parseResult, test)
  }

  it should "parse a place for a Wall correctly" in {
    ActionParser("place wall (1,3)")
  }

  it should "parse a place for a Capstone correctly" in {
    ActionParser("place capstone (1,3)")
  }

  it should "allow additional worlds in place instructions" in {
    ActionParser("place a minion at (1,3)")
  }

  it should "detect invalid token kinds" in {
    ActionParser("place a woggle at (1,3)")
  }

  it should "allow different ways of notation for a position" in {
    ActionParser("place a minion at Position(1,3)")
    ActionParser("place a minion at (1,3)")
    ActionParser("place a minion at (1, 3)")
    ActionParser("place a minion at (1    , 3)")
    ActionParser("place a minion at (  1    , 3  )")
    ActionParser("place a minion at Position(  1    , 3  )")
  }
}



