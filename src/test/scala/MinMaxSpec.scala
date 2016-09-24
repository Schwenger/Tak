
import ai.evaluation.{Evaluator, TokenCount}
import ai.search.MinMax
import org.scalatest._
import parsing.state.StateDeserializer
import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}
import simulator.interfaces.elements._
import simulator.interfaces.{ActionExecutor, GameLogic, GameState, PlayerColor}

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

      val eval = TokenCount

      val state = (str: String) => StateDeserializer(str)

      val supplier: PlayerColor => GameState => Seq[Action] = (color: PlayerColor) => GameLogic.availableActions(_)(color)

    }

//  override def apply(state: GameState, eval: Evaluator, actionSupplier: (GameState) => Seq[Action],
//                     depth: Int, maxPlayer: PlayerColor): Action = {

  "MinMax" should "compute the right move for red with depth 1" in {
    val f = fixture
    val c = Red
    val stateStr =
      """
        | RM & RM
        | RM & _
      """.stripMargin
    val a = MinMax(f.state(stateStr), f.eval, f.supplier(c), 1, c)
    a shouldBe PlaceMinion(Position(1,1))
  }

  it should "output the winning move for red with depth 1" in {
    val f = fixture
    val c = Red

    val stateStr1 =
      """
        | RM & _
        | BM & _
      """.stripMargin
    val a1 = MinMax(f.state(stateStr1), f.eval, f.supplier(c), 1, c)
    a1 shouldBe PlaceMinion(Position(1,1))

    val stateStr2 =
      """
        | BM & _
        | RM & _
      """.stripMargin
    val a2 = MinMax(f.state(stateStr2), f.eval, f.supplier(c), 1, c)
    a2 shouldBe PlaceMinion(Position(0,1))

    val stateStr3 =
      """
        | RM & BM
        | _  & _
      """.stripMargin
    val a3 = MinMax(f.state(stateStr3), f.eval, f.supplier(c), 1, c)
    a3 shouldBe PlaceMinion(Position(1,1))

    val stateStr4 =
      """
        | BM & RM
        | _ & _
      """.stripMargin
    val a4 = MinMax(f.state(stateStr4), f.eval, f.supplier(c), 1, c)
    a4 shouldBe PlaceMinion(Position(1,1))

  }
}
