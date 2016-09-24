import org.scalatest.{FlatSpec, Matchers}
import simulator.interfaces.PlayerColor.{Black, Red}
import simulator.interfaces.GameState
import simulator.interfaces.elements._
import simulator.logic.GameOver

class GameLogicGameOverSpec extends FlatSpec with Matchers {
  def fixture =
    new {
        /*
          y __ __ __ __
          3|RM|__|RM|RM|
          2|RM|RM|__|RM|
          1|BM|BM|__|__|
          0|__|BM|BM|BM|
             0  1  2  3 x
         */
        /*
                y _ _ _ _
                3|R|B|R|R|
                2|R|R|B|R|
                1|B|B|R|R|
                0|B|R|B|B|
                  0 1 2 3 x
       */
      val board_size = 4
      val redWall = Wall(Red)
      val redMin = Minion(Red)
      val redCap = Capstone(Red)
      val blackCap = Capstone(Black)
      val blackMin = Minion(Black)
      val blackWall = Wall(Black)

      val _state = new GameState(board_size)
      _state.setField(Position(0,3), redMin)
      _state.setField(Position(0,2), redMin)
      _state.setField(Position(0,1), blackMin)
      _state.setField(Position(1,2), redMin)
      _state.setField(Position(1,1), blackMin)
      _state.setField(Position(1,0), blackMin)
      _state.setField(Position(2,3), redMin)
      _state.setField(Position(2,0), blackMin)
      _state.setField(Position(3,3), redMin)
      _state.setField(Position(3,2), redMin)
      _state.setField(Position(3,0), blackMin)

      val _state2 = new GameState(board_size)
      _state2.setField(Position(0,0), blackMin)
      _state2.setField(Position(0,1), blackMin)
      _state2.setField(Position(0,2), redMin)
      _state2.setField(Position(0,3), redMin)
      _state2.setField(Position(1,0), redMin)
      _state2.setField(Position(1,1), blackMin)
      _state2.setField(Position(1,2), redMin)
      _state2.setField(Position(1,3), blackMin)
      _state2.setField(Position(2,0), blackMin)
      _state2.setField(Position(2,1), redMin)
      _state2.setField(Position(2,2), blackMin)
      _state2.setField(Position(2,3), redMin)
      _state2.setField(Position(3,0), blackMin)
      _state2.setField(Position(3,1), redMin)
      _state2.setField(Position(3,2), redMin)
      _state2.setField(Position(3,3), redMin)

      def state = _state.copy
      def state2 = _state2.copy
  }

  "Game Over Evaluator" should "not count diagonals" in {
  /*
          y __ __ __ __
          3|RM|__|RM|RM|
          2|RM|RM|__|RM|
          1|BM|__|__|__|
          0|__|BM|BM|BM|
             0  1  2  3 x
  */
    val f = fixture
    val state = f.state
    state.clearField(Position(1,1))
    GameOver(state) should be (None)
  }

  it should "identify the street" in {
    /*
              y __ __ __ __
              3|RM|__|RM|RM|
              2|RM|RM|__|RM|
              1|BM|BM|__|__|
              0|__|BM|BM|BM|
                 0  1  2  3 x
     */
    val f = fixture
    val state = f.state
    GameOver(state) should be (Some(Black))
  }

  it should "not count Walls for streets" in {
    /*
              y __ __ __ __
              3|RM|__|RM|RM|
              2|RM|RM|__|RM|
              1|BM|BM|__|__|
              0|__|BM|BW|BM|
                 0  1  2  3 x
     */
    val f = fixture
    val state = f.state
    state.clearField(Position(2,0))
    state.setField(Position(2,0), f.blackWall)
    GameOver(state) should be (None)
  }

  it should "count Capstones for streets" in {
    /*
              y __ __ __ __
              3|RM|__|RM|RM|
              2|RM|RM|__|RM|
              1|BM|BM|__|__|
              0|__|BM|BC|BM|
                 0  1  2  3 x
     */
    val f = fixture
    val state = f.state
    state.clearField(Position(2,0))
    state.setField(Position(2,0), f.blackCap)
    GameOver(state) should be (Some(Black))
  }

  it should "detect an invalid situation" in {
    /*
              y __ __ __ __
              3|RM|RM|RM|RM|
              2|RM|RM|__|RM|
              1|BM|BM|__|__|
              0|__|BM|BW|BM|
                 0  1  2  3 x
     */
    val f = fixture
    val state = f.state
    state.setField(Position(1,3), f.redMin)
    a [IllegalStateException] should be thrownBy {
      GameOver(state)
    }
  }



  it should "identity the winner when there is a full board" in {
    /*
              y _ _ _ _
              3|R|B|R|R|
              2|R|R|B|R|
              1|B|B|R|R|
              0|B|R|B|B|
                0 1 2 3 x
     */
    val f = fixture
    val state = f.state2
    GameOver(state) should be (Some(Red))
  }

  it should "not count Capstones for points when the board is full" in {
    /*
              y __ __ __ __
              3|RC|B |RC|R |
              2|R |RC|B |RC|
              1|B |B |R |R |
              0|B |R |B |B |
                0  1  2  3 x
     */
    val f = fixture
    val state = f.state2
    state.clearField(Position(0,3))
    state.setField(Position(0,3), f.redCap)
    state.clearField(Position(1,2))
    state.setField(Position(1,2), f.redCap)
    state.clearField(Position(2,3))
    state.setField(Position(2,3), f.redCap)
    state.clearField(Position(3,2))
    state.setField(Position(3,2), f.redCap)

    GameOver(state) should be (Some(Black))
  }

  it should "not count Walls for points when the board is full" in {
    /*
              y __ __ __ __
              3|RW|B |RW|R |
              2|R |RW|B |RW|
              1|B |B |R |R |
              0|B |R |B |B |
                0  1  2  3 x
     */
    val f = fixture
    val state = f.state2
    state.clearField(Position(0,3))
    state.setField(Position(0,3), f.redWall)
    state.clearField(Position(1,2))
    state.setField(Position(1,2), f.redWall)
    state.clearField(Position(2,3))
    state.setField(Position(2,3), f.redWall)
    state.clearField(Position(3,2))
    state.setField(Position(3,2), f.redWall)

    GameOver(state) should be (Some(Black))
  }

  it should "detect a draw and let black win" in {
    /*
              y __ __ __ __
              3|RW|B |R |R |
              2|R |RW|B |R |
              1|B |B |R |R |
              0|B |R |B |B |
                0  1  2  3 x
     */
    val f = fixture
    val state = f.state2
    // it was 7 v. 9, not it is 7 v. 7
    state.clearField(Position(0,3))
    state.setField(Position(0,3), f.redWall)
    state.clearField(Position(1,2))
    state.setField(Position(1,2), f.redWall)

    GameOver(state) should be (Some(Black))
  }

  it should "detect even the most tangled street" in {
    /*
        y  ___ ___ ___ ___ ___
        4 | x | x | x | x | O |
        3 | O | O | O | x | O |
        2 | O | x | O | x | O |
        1 | O | x | O | O | O |
        0 | O | x | x | x | x |
            0   1   2   3   4 x
     */
    val f = fixture
    val state = new GameState(5)
    state.setField(Position(0,0), f.blackMin)
    state.setField(Position(0,1), f.blackMin)
    state.setField(Position(0,2), f.blackMin)
    state.setField(Position(0,3), f.blackMin)
    state.setField(Position(0,4), f.redMin)
    state.setField(Position(1,0), f.redMin)
    state.setField(Position(1,1), f.redMin)
    state.setField(Position(1,2), f.redMin)
    state.setField(Position(1,3), f.blackMin)
    state.setField(Position(1,4), f.redMin)
    state.setField(Position(2,0), f.redMin)
    state.setField(Position(2,1), f.blackMin)
    state.setField(Position(2,2), f.blackMin)
    state.setField(Position(2,3), f.blackMin)
    state.setField(Position(2,4), f.redMin)
    state.setField(Position(3,0), f.redMin)
    state.setField(Position(3,1), f.blackMin)
    state.setField(Position(3,2), f.redMin)
    state.setField(Position(3,3), f.redMin)
    state.setField(Position(3,4), f.redMin)
    state.setField(Position(4,0), f.redMin)
    state.setField(Position(4,1), f.blackMin)
    state.setField(Position(4,2), f.blackMin)
    state.setField(Position(4,3), f.blackMin)
    state.setField(Position(4,4), f.blackMin)

    GameOver(state) should be (Some(Black))
  }
}
