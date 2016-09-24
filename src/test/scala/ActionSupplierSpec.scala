import org.scalatest.{FlatSpec, Matchers}
import parsing.state.StateDeserializer
import simulator.analysis.ActionSupplier
import simulator.interfaces.PlayerColor
import simulator.interfaces.elements._

class ActionSupplierSpec extends FlatSpec with Matchers {

  def fixture = new {

  }

  // INVALID

  "ActionSupplier" should "not invent slides where there are none" in {
    val descr =
      """
        |_ & _
        |_ & _
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Slide)(PlayerColor.Red)
    actions should have length 0
  }

  it should "not invent moves where there are none" in {
    val descr =
      """
        |_ & _
        |_ & _
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Red)
    actions should have length 0
  }

  it should "not invent places where there are none" in {
    val descr =
      """
        |RM & RM
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Place)(PlayerColor.Red)
    actions should have length 0
  }

  // SURRENDER

  it should "yield surrender" in {
    val descr =
      """
        |_ & _
        |_ & _
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Surrender)(PlayerColor.Red)
    actions should have length 1
    actions.head should be (Surrender())
  }

  // PLACES

  it should "detect places" in {
    val descr =
      """
        |RM & RM
        |_  & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Place)(PlayerColor.Red)
    actions should have length 2
    actions should contain (PlaceMinion(Position(0,0)))
    actions should contain (PlaceWall(Position(0,0)))
  }

  it should "allow placing Capstones" in {
    val descr =
      """
        |RM & RM & RM & RM & RM
        |RM & RM & RM & RM & RM
        |RM & RM & RM & RM & RM
        |RM & RM & RM & RM & RM
        |_  & RM & RM & RM & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Place)(PlayerColor.Red)
    actions should have length 3
    actions should contain (PlaceMinion(Position(0,0)))
    actions should contain (PlaceWall(Position(0,0)))
    actions should contain (PlaceCapstone(Position(0,0)))
  }

  // MOVES

  it should "detect moves" in {
    val descr =
      """
        |BM & RM
        |_  & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Black)
    actions should have length 2
    actions should contain (Move(Position(0,1), Direction.Down))
    actions should contain (Move(Position(0,1), Direction.Right))
  }

  it should "distinguish colors when looking for moves" in {
    val descr =
      """
        |RM & RM
        |_  & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Black)
    actions should have length 0
  }

  it should "detect blocking fields" in {
    val descr =
      """
        |RW & RM
        |BM & RW
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Black)
    actions should have length 0
  }

  it should "allow Capstones to crush" in {
    val descr =
      """
        |RW & RM
        |BC & RW
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Black)
    actions should have length 2
    actions should contain (Move(Position(0,0), Direction.Right))
    actions should contain (Move(Position(0,0), Direction.Up))
  }

  it should "allow Capstones to crush unless there are other Capstones" in {
    val descr =
      """
        |RC & RM
        |BC & RW
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Move)(PlayerColor.Black)
    actions should have length 1
    actions.head should be (Move(Position(0,0), Direction.Right))
  }

  // SLIDES

  it should "not make slides up" in {
    val descr =
      """
        |RM & RM
        |_  & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Slide)(PlayerColor.Red)
    actions should have length 0
  }

  it should "allow slides for stacks of size 2" in {
    val descr =
      """
        |Stack(RM,RM) & _
        |_            & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Slide)(PlayerColor.Red)
    actions should have length 4
    actions should contain (Slide(Position(0,1), List(1), Direction.Down))
    actions should contain (Slide(Position(0,1), List(1), Direction.Right))
    actions should contain (Slide(Position(0,1), List(2), Direction.Down))
    actions should contain (Slide(Position(0,1), List(2), Direction.Right))
  }

  it should "respect domination when sliding" in {
    val descr =
      """
        |Stack(RM,RM) & _
        |_            & RM
      """.stripMargin
    val state = StateDeserializer(descr)
    val actions = ActionSupplier(state, ActionKind.Slide)(PlayerColor.Black)
    actions should have length 0
  }
}
