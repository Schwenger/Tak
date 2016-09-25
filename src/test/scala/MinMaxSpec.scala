
import ai.evaluation.{Domination, GoalIndicator}
import ai.search.MinMax
import org.scalatest._
import parsing.state.StateDeserializer
import simulator.GameState
import simulator.analysis.ActionSupplier
import simulator.elements._
import simulator.PlayerColor.{PlayerColor, Red}

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

    }

  "MinMax" should "place a minion to increase the dominance" in {
    val f = fixture
    val c = Red
    val eval = Domination(c)
    val stateStr =
      """
        | RM & RM
        | RM & _
      """.stripMargin
    val a = MinMax(f.state(stateStr), eval, f.supplier, 1, c)
    val option1 = a == PlaceMinion(Position(1,0))
    val option2 = a == PlaceWall(Position(1,0))
    option1 || option2 should be (true)
  }

  it should "output the winning move for red with depth 1" in {
    val f = fixture
    val c = Red
    val eval = GoalIndicator(c)

    val stateStr1 =
      """
        | RM & _
        | BM & _
      """.stripMargin
    val a1 = MinMax(f.state(stateStr1), eval, f.supplier, 1, c)
    val option11 = a1 == PlaceMinion(Position(1,1))
    val option12 = a1 == PlaceWall(Position(1,1))
    option11 || option12 should be (true)

    val stateStr2 =
      """
        | BM & _
        | RM & _
      """.stripMargin
    val a2 = MinMax(f.state(stateStr2), eval, f.supplier, 1, c)
    a2 shouldBe PlaceMinion(Position(1,0))

    val stateStr3 =
      """
        | RM & BM
        | _  & _
      """.stripMargin
    val a3 = MinMax(f.state(stateStr3), eval, f.supplier, 1, c)
    a3 shouldBe PlaceMinion(Position(0,0))

    val stateStr4 =
      """
        | BM & RM
        | _ & _
      """.stripMargin
    val a4 = MinMax(f.state(stateStr4), eval, f.supplier, 1, c)
    val option41 = a4 == PlaceMinion(Position(1,0))
    val option42 = a4 == PlaceWall(Position(1,0))
    option41 || option42 should be (true)

  }

  it should "prevent a loss by blocking the enemy's road" in {
    val f = fixture
    val c = Red
    val eval = GoalIndicator(c)

    val stateStr1 =
      """
        | _  & BM & _
        | _  & _  & _
        | _  & BM & _
      """.stripMargin
    val a1 = MinMax(f.state(stateStr1), eval, f.supplier, 2, c)
    val option1 = a1 == PlaceMinion(Position(1,1))
    val option2 = a1 == PlaceWall(Position(1,1))
    option1 || option2 should be (true)
  }

  it should "prevent a loss sliding in the way even though this means losing dominance" in {
    val f = fixture
    val c = Red
    val eval = GoalIndicator(c)

    val stateStr1 =
      """
        | BM & BC & _
        | _  & RC & _
        | _  & _  & Stack(RC,BM)
      """.stripMargin
    val state = f.state(stateStr1)
    for (i <- 0 until state.minionsLeft(c))
      state.removeToken(c, minion = true)
    val a1 = MinMax(state, eval, f.supplier, 2, c)
    a1 should be (Slide(Position(2, 0), List(2,1), Direction.Up))
  }


}
