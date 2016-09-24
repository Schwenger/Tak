package simulator.interfaces

import java.util.concurrent.atomic.AtomicInteger

import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}
import simulator.interfaces.elements.{Action, PlaceMinion}
import simulator.logic.GameOver

import scala.collection.mutable

// In the fair mode, the black player has a chance to take a last action when a winning condition is met.
class Simulation(red: Player, black: Player, size: Int) {

  var state: GameState = new GameState(size)
  var turn = 0
  private var turns: List[PlayerMapping[Action]] = Nil

  def simulateTurn() = {
    assert(turns.length == turn)

    val checksum = state.checksum
    val rAction = red.nextAction(turns.head, state)
    assert(checksum == state.checksum)
    state = ActionExecutor(rAction, state, Red)

    if(gameOver.isDefined) {
      turns ::= PlayerMapping(rAction, null) // not particularly pretty.
    } else {
      val half_turn = PlayerMapping(red = rAction, black = turns.head.black)

      val checksum = state.checksum
      val bAction = black.nextAction(half_turn, state)
      assert(checksum == state.checksum)
      state = ActionExecutor(bAction, state, Black)

      turns ::= PlayerMapping(rAction, bAction)
    }
    turn += 1
  }

  def simulateFirstTurn() = {
    red.init(state)
    black.init(state)
    val pos4black = red.firstAction(state)
    val pseudoBMove = PlaceMinion(pos4black)
    state = ActionExecutor(pseudoBMove, state, Black)
    val pos4red= black.firstAction(state)
    val pseudoRMove = PlaceMinion(pos4red)
    state = ActionExecutor(pseudoRMove, state, Red)
    turns ::= PlayerMapping(pseudoRMove, pseudoBMove)
    turn += 1
  }

  def gameOver: Option[PlayerColor] = GameOver(state)

}

object Simulator {

  val simulations = new mutable.MutableList[Simulation]
  val running = new AtomicInteger(0)
  val over = new AtomicInteger(0)
  val won = (new AtomicInteger(0), new AtomicInteger(0))

  /**
    * Simulates a whole game.
    * @param red Starting Player
    * @param black Second Player
    * @return results are provided to the players directly using the respective accept method
    */
  def apply(red: Player, black: Player, size: Int = 4) = {
      val sim = new Simulation(red, black, size)
      var result: Option[PlayerColor] = None

      sim.simulateFirstTurn()
      do {
        sim.simulateTurn()
        result = sim.gameOver
      } while (result.isEmpty)

      running.decrementAndGet()
      over.incrementAndGet()
      result.get match {
        case Red => won._1.incrementAndGet()
        case Black => won._2.incrementAndGet()
      }
  }
}

