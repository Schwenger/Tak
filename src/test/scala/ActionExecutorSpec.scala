
import org.scalatest._
import simulator.{GameState, PlayerColor}
import simulator.elements._
import simulator.PlayerColor.{Black, Red}
import simulator.logic.ActionExecutor

class ActionExecutorSpec extends FlatSpec with Matchers {

  def fixture =
    new {
      /*
        y _ _ _ _
        3|_|_|W|_|
        2|_|_|_|_|
        1|_|_|_|_|
        0|S|_|_|W|
          0 1 2 3 x
       */
      val redWall = Wall(PlayerColor.Red)
      val redMin = Minion(PlayerColor.Red)
      val redCap = Capstone(PlayerColor.Red)
      val blackCap = Capstone(PlayerColor.Black)
      val blackMin = Minion(PlayerColor.Black)
      val blackWall = Wall(PlayerColor.Black)
      val redStack1 = Stack(List(redCap, blackMin, redMin, blackMin))
      val redStack2 = Stack(List(redCap, blackMin, redMin, blackMin, redMin))
      val state = new GameState(4)
      state.setField(Position(0,0), redStack1)
      state.setField(Position(3,0), blackWall)
      state.setField(Position(2,3), redWall)
      val state2 = new GameState(5)
      state2.setField(Position(0,0), redStack2)
    }

  "ActionExecutor" should "spread the stack appropriately" in {
    val f = fixture
    val action = Slide(Position(0,0), List(4,2,1), Direction.Right)
    val state = ActionExecutor(action, f.state, Red)

    state(0,0) should be (None)

    state(1,0) shouldBe 'isDefined
    state(1,0).get shouldBe a [Stack]
    state(1,0).get.asInstanceOf[Stack].content should have length 2
    state(1,0).get.asInstanceOf[Stack].content.head should be (f.redMin)
    state(1,0).get.asInstanceOf[Stack].content(1) should be (f.blackMin)

    state(2,0) shouldBe 'isDefined
    state(2,0).get should be (f.blackMin)

    state(3,0) shouldBe 'isDefined
    state(3,0).get shouldBe a [Stack]
    state(3,0).get.asInstanceOf[Stack].content should have length 2
    state(3,0).get.asInstanceOf[Stack].content.head should be (f.redCap)
    state(3,0).get.asInstanceOf[Stack].content(1) should be (Minion(PlayerColor.Black))
  }

  it should "execute moves properly" in {
    val f = fixture
    val action = Move(Position(2,3), Direction.Right)
    val state = ActionExecutor(action, f.state, Red)

    state(2, 3) shouldBe 'isEmpty
    state(3, 3).get should be (f.redWall)
  }

  it should "stack tokens properly when moving" in {
    val f = fixture
    val action = Move(Position(3,1), Direction.Down)
    val old = f.state.copy
    old.setField(Position(3,1), f.redCap)
    val state = ActionExecutor(action, old, Red)

    state(3, 1) shouldBe 'isEmpty
    state(3, 0).get should be (Stack(List(f.redCap, f.blackMin)))

  }

  it should "not swallow stones but leave them behind" in {
    val f = fixture
    val action = Slide(Position(0,0), List(4,2,1), Direction.Right)
    val state = ActionExecutor(action, f.state2, Red)

    state(0,0) shouldBe 'isDefined
    state(0,0).get should be (f.redMin)

  }

  it should "place Minions correctly" in {
    val f = fixture
    val target = Position(3,2)
    val action = PlaceMinion(target)
    val state = ActionExecutor(action, f.state, Red)
    state(target) shouldBe 'isDefined
    state(target).get should be (Minion(Red))
  }

  it should "place Capstone correctly" in {
    val f = fixture
    val target = Position(3,2)
    val action = PlaceCapstone(target)
    val state = ActionExecutor(action, f.state2, Red)
    state(target) shouldBe 'isDefined
    state(target).get should be (Capstone(Red))
  }

  it should "place Wall correctly" in {
    val f = fixture
    val target = Position(3,2)
    val action = PlaceWall(target)
    val state = ActionExecutor(action, f.state, Black)
    state(target) shouldBe 'isDefined
    state(target).get should be (Wall(Black))
  }

}
