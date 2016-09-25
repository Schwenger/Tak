
import org.scalatest._
import simulator.{GameState, PlayerColor}
import simulator.elements._
import simulator.PlayerColor.{Black, Red}
import simulator.logic.ActionValidator

class GameLogicValidMoveSpec extends FlatSpec with Matchers {

  def fixture =
    new {
      /*
        y _ _ _ _
        3|_|_|M|S|
        2|_|_|_|W|
        1|W|_|_|_|
        0|S|_|_|_|
          0 1 2 3 x
       */
      val board_size = 4
      val redWall = Wall(PlayerColor.Red)
      val redMin = Minion(PlayerColor.Red)
      val redCap = Capstone(PlayerColor.Red)
      val blackCap = Capstone(PlayerColor.Black)
      val blackMin = Minion(PlayerColor.Black)
      val blackWall = Wall(PlayerColor.Black)
      val redStack1 = Stack(List(redCap, blackMin, redMin, blackMin))
      val redStack2 = Stack(List(redCap, blackMin))
      val stackPos = Position(0,0)
      val state = new GameState(board_size)
      state.setField(stackPos, redStack1)
      state.setField(Position(0,1), blackWall)
      state.setField(Position(2,3), redWall)
      state.setField(Position(3,3), redStack2)
      state.setField(Position(3,2), redWall)
      val board_size_2 = 5
      val state2 = new GameState(board_size_2)
      state2.setField(Position(0,0), redStack2)
    }

  // SLIDES
  "The Move Validator" should "not allow slides with too many stones" in {
    val f = fixture
    val action = Slide(f.stackPos, List(8,1), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow slides with more stones than in the original stack" in {
    val f = fixture
    val action = Slide(f.stackPos, List(4, 1), Direction.Right)
    ActionValidator(f.state2, action)(Red) should be (false)
  }

  it should "not allow slides with non-decreasing stone values" in {
    val f = fixture
    val action = Slide(f.stackPos, List(2,3,1), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow to skip one position in a slide" in {
    val f = fixture
    val action = Slide(f.stackPos, List(4,0,1), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow sliding off the board" in {
    val f = fixture
    val action = Slide(f.stackPos, List(2), Direction.Left)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow slides rolling over blocking fields" in {
    val f = fixture
    val action = Slide(f.stackPos, List(3,1), Direction.Up)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow stacks with Capstones to be crushing" in {
    val f = fixture
    val action = Slide(f.stackPos, List(3), Direction.Up)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow slides from fields that are not dominated by the active player " in {
    val f = fixture
    val action = Slide(f.stackPos, List(3), Direction.Right)
    ActionValidator(f.state, action)(Black) should be (false)
  }

  // MOVE

  it should "not allow moves from an empty field" in {
    val f = fixture
    val action = Move(Position(0,3), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (false)
    ActionValidator(f.state, action)(Black) should be (false)
  }

  it should "allow valid moves" in {
    val f = fixture
    val action = Move(Position(3,2), Direction.Left)
    ActionValidator(f.state, action)(Red) should be (true)
  }

  it should "detect invalid destinations" in {
    val f = fixture
    val action = Move(Position(3,2), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "detect if the field is not dominated by the respective player" in {
    val f = fixture
    val action = Move(Position(3,2), Direction.Left)
    ActionValidator(f.state, action)(Black) should be (false)
  }

  // VALID CASES
  it should "allow valid slides" in {
    val f = fixture
    val action = Slide(f.stackPos, List(3,1), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (true)
  }

  it should "allow valid places" in {
    val f = fixture
    val actions = Seq(PlaceMinion(Position(2,0)), PlaceCapstone(Position(1,1)), PlaceWall(Position(0,3)))
    for(action <- actions)
      ActionValidator(f.state2, action)(Black) should be (true)
  }

  it should "allow capstones to be crushing as the last step of a slide" in {
    val f = fixture
    val action = Slide(f.stackPos, List(4,3,1), Direction.Right)
    ActionValidator(f.state, action)(Red) should be (true)
  }

  it should "allow capstones to be crushing as the first step of a slide" in {
    val f = fixture
    val action = Slide(Position(3,3), List(1), Direction.Down)
    ActionValidator(f.state, action)(Red) should be (true)
  }

  // PLACES
  it should "not allow places on occupied fields" in {
    val f = fixture
    val action = PlaceCapstone(f.stackPos)
    ActionValidator(f.state, action)(Red) should be (false)
  }

  it should "not allow placing tokens off the board" in {
    val f = fixture
    val action1 = PlaceMinion(Position(f.board_size, 2))
    ActionValidator(f.state, action1)(Red) should be (false)
    val action2 = PlaceMinion(Position(2, f.board_size))
    ActionValidator(f.state, action2)(Black) should be (false)
  }

  it should "not allow placing tokens after running out of tokens" in {
    val f = fixture
    val initMinions = f.state2.minionsLeft(Red)
    for(i <- 1 to initMinions)
      f.state2.removeToken(Red, minion = true)
    val initCapstones = f.state2.capstonesLeft(Red)
    for(i <- 1 to initCapstones)
      f.state2.removeToken(Red, minion = false)

    val action1 = PlaceMinion(Position(2,0))
    val action2 = PlaceCapstone(Position(2,0))

    ActionValidator(f.state2, action1)(Red) should be (false)
    ActionValidator(f.state2, action2)(Red) should be (false)
  }


}
