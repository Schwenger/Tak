import org.scalatest.{FlatSpec, Matchers}
import parsing.action.{Compiler, Lexer, Parser}
import simulator.interfaces.elements._

import scala.util.Try


class ActionCompilerSpec extends FlatSpec with Matchers {

  def fixture = new {
    def run(cmd: String): Try[Action] = Lexer(cmd) flatMap (Parser(_)) flatMap (Compiler(_))
    def check(is: Try[Action], expected: Action): Unit = {
      if(is.isFailure)
        throw is.failed.get
      is.isSuccess should be (true)
      is.get should be (expected)
    }
    def runAndCheck(cmd: String, expected: Action): Unit = {
      check(run(cmd), expected)
    }
  }

  "ActionCompiler" should "compile moves correctly" in {
    val f = fixture
    f.runAndCheck("move (0,3) (1,3)", Move(Position(0,3), Direction.Right))
  }

  it should "detect invalid moves" in {
    val f = fixture
    val res = f.run("move (0,3) (1,1)")
    res.isFailure should be (true)
  }

  it should "allow surrendering" in {
    val f = fixture
    f.runAndCheck("surrender", Surrender())
  }

  it should "allow surrendering in two words" in {
    val f = fixture
    f.runAndCheck("give up", Surrender())
  }

  it should "allow sliding" in {
    val f = fixture
    f.runAndCheck("slide Position(3,2) north drop 5, 2, 1", Slide(Position(3,2), List(5,2,1), Direction.Up))
  }

  it should "allow moves with a direction" in {
    val f = fixture
    f.runAndCheck("move Position(0,0) up", Move(Position(0,0), Direction.Up))
  }

  it should "allow placing capstones" in {
    val f = fixture
    f.runAndCheck("place capstone at (0,2)", PlaceCapstone(Position(0,2)))
  }

  it should "allow placing walls" in {
    val f = fixture
    f.runAndCheck("place wall at (0,2)", PlaceWall(Position(0,2)))
  }

  it should "allow placing minions" in {
    val f = fixture
    f.runAndCheck("place a minion at (0,2)", PlaceMinion(Position(0,2)))
  }

}
