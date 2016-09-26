
import ai.evaluation.{DefaultInterpolator, Domination, GoalIndicator}
import ai.search.MinMax
import org.scalatest._
import parsing.state.StateDeserializer
import simulator.GameState
import simulator.elements._
import simulator.PlayerColor.{PlayerColor, Red}
import simulator.logic.ActionSupplier

class MinMaxSpec extends FlatSpec with Matchers {

  def fixture =
    new {
      /*
        y _ _ _ _
        3|_|_|M|_|
        2|_|_|_|_|
        1|_|_|_|_|
        0|S|_|_|W|
          0 1 2 3 x
       */

      val state = (str: String) => StateDeserializer(str)

      val supplier: (GameState, PlayerColor) => Seq[Action] = (state, color) => ActionSupplier(state)(color)

      val eval = DefaultInterpolator.allEqual

    }

  "MinMax" should "place a minion to increase the dominance" in {
    val f = fixture
    val c = Red
    val stateStr =
      """
        | RM & RM
        | RM & _
      """.stripMargin
    val action = MinMax(f.state(stateStr), f.eval(c), f.supplier, 1, c)
    val resSpace = List(PlaceMinion(Position(1,0)), PlaceWall(Position(1,0)))
    resSpace should contain (action)
  }

  it should "output the winning move for red with depth 1" in {
    val f = fixture
    val c = Red

    val stateStr1 =
      """
        | RM & _
        | BM & _
      """.stripMargin
    val a1 = MinMax(f.state(stateStr1), f.eval(c), f.supplier, 1, c)
    val resSpace1 = List(PlaceMinion(Position(1,1)), PlaceWall(Position(1,1)))
    resSpace1 should contain (a1)

    val stateStr2 =
      """
        | BM & _
        | RM & _
      """.stripMargin
    val a2 = MinMax(f.state(stateStr2), f.eval(c), f.supplier, 1, c)
    a2 shouldBe PlaceMinion(Position(1,0))

    val stateStr3 =
      """
        | RM & BM
        | _  & _
      """.stripMargin
    val a3 = MinMax(f.state(stateStr3), f.eval(c), f.supplier, 1, c)
    a3 shouldBe PlaceMinion(Position(0,0))

    val stateStr4 =
      """
        | BM & RM
        | _ & _
      """.stripMargin
    val a4 = MinMax(f.state(stateStr4), f.eval(c), f.supplier, 1, c)
    val resSpace4 = List(PlaceMinion(Position(1,0)), PlaceWall(Position(1,0)))
    resSpace4 should contain (a4)
  }

  it should "prevent a loss by blocking the enemy's road" in {
    val f = fixture
    val c = Red

    val stateStr1 =
      """
        | _  & BM & _
        | _  & _  & _
        | _  & BM & _
      """.stripMargin
    val a1 = MinMax(f.state(stateStr1), f.eval(c), f.supplier, 2, c)
    val resSpace = List(PlaceMinion(Position(1,1)), PlaceWall(Position(1,1)))
    resSpace should contain (a1)
  }

  it should "prevent a loss sliding in the way even though this means losing dominance" in {
    val f = fixture
    val c = Red

    val stateStr1 =
      """
        | BM & BC & _
        | _  & RC & _
        | _  & _  & Stack(RC,BM)
      """.stripMargin
    val state = f.state(stateStr1)
    for (i <- 0 until state.minionsLeft(c))
      state.removeToken(c, minion = true)
    val a1 = MinMax(state, f.eval(c), f.supplier, 2, c)
    a1 should be (Slide(Position(2, 0), List(2,1), Direction.Up))
  }


}
