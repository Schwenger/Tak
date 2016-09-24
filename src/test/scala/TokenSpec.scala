
import org.scalatest._
import simulator.interfaces.PlayerColor
import simulator.interfaces.elements.{Capstone, Minion, Stack, Wall}

class TokenSpec extends FlatSpec with Matchers {

  def fixture =
    new {
      val wall = Wall(PlayerColor.Red)
      val min = Minion(PlayerColor.Red)
      val cap = Capstone(PlayerColor.Red)
      val blackC = Capstone(PlayerColor.Black)
      val blackM = Minion(PlayerColor.Black)
    }

  "Token" should "throw an assertion error if something is stacked upon a wall" in {
    val f = fixture
    an [AssertionError] should be thrownBy {
      f.min :: f.wall
    }
  }

  it should "crush a wall when stacking a capstone on top of it" in {
    val f = fixture
    val t = f.cap :: f.wall
    t shouldBe a [Stack]
    val content = t.asInstanceOf[Stack].content
    content should have length 2
    content.head should be (f.cap)
    content(1) shouldBe a [Minion]
  }

  it should "merge two stacks correctly, i.e. their lengths add and the color fits" in {
    val f = fixture
    val first = f.blackC
    val second = f.min
    val third = f.min
    val fourth = f.min
    val fifth = f.min
    val sixth = f.blackM
    val seventh = f.min
    val bot = Stack(List(fifth, sixth, seventh))
    val top = Stack(List(first, second, third, fourth))
    val res = top :: bot
    res shouldBe a [Stack]
    res shouldBe 'blocking
    res should not be 'crushing
    res should not be 'crushable
    res.player should be (first.player)
    val content = res.asInstanceOf[Stack].content
    content should have length 7
    content.head should be (first)
    content(1) should be (second)
    content(2) should be (third)
    content(3) should be (fourth)
    content(4) should be (fifth)
    content(5) should be (sixth)
    content(6) should be (seventh)
  }

  it should "prevent Capstone from crushing if part of a stack" in {
    val f = fixture
    val bot = Stack(List(f.wall, f.blackM, f.min))
    val top = Stack(List(f.blackC, f.min, f.min, f.min))
    a [AssertionError] should be thrownBy {
      top :: bot
    }
  }

}
