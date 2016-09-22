import org.scalatest.{FlatSpec, Matchers}
import parsing.state.StateDeserializer
import simulator.interfaces.PlayerColor.{Black, Red}
import simulator.interfaces.game_elements.{Capstone, Minion, Stack, Wall}

class StateDeserializerSpec extends FlatSpec with Matchers {

  "StateDeserializer" should "recognize minions" in {
    val board =
      """
        |RM & RM
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (Some(Minion(Red)))
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "recognize empty fields" in {
    val board =
      """
        |RM & _
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (None)
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "cope with spaces" in {
    val board =
      """
        |RM  &    _
        |  RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (None)
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "recognize walls" in {
    val board =
      """
        |RM & RW
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (Some(Wall(Red)))
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "recognize capstones" in {
    val board =
      """
        |RM & RC
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (Some(Capstone(Red)))
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "distinguish betwee colors" in {
    val board =
      """
        |RM & RM
        |BM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) should be (Some(Minion(Red)))
    state(1, 0) should be (Some(Minion(Black)))
    state(1, 1) should be (Some(Minion(Red)))
  }

  it should "read stacks" in {
    val board =
      """
        |RM & Stack(RM,BM)
        |RM & RM
      """.stripMargin
    val state = StateDeserializer(board)
    state(0, 0) should be (Some(Minion(Red)))
    state(0, 1) shouldBe 'defined
    state(0, 1).get shouldBe a [Stack]
    val content = state(0, 1).get.asInstanceOf[Stack].content
    content.head should be (Minion(Red))
    content(1) should be (Minion(Black))
    state(1, 0) should be (Some(Minion(Red)))
    state(1, 1) should be (Some(Minion(Red)))
  }

}
