package simulator.interfaces

import java.util.concurrent.atomic.AtomicInteger

import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}
import simulator.interfaces.game_elements.Move
import simulator.logic.ActionExecutor
import simulator.model.GameState

import scala.collection.mutable

// TODO implement two rule sets: Fair and unfair.
// In the fair mode, the black player has a chance to take a last action when a winning condition is met.
class Simulation(red: Player, black: Player) {
  val state = new GameState
  var turn = 0

  def simulateTurn() = {

  }

  def simulateFirstTurn() = {
  }

  def gameOver: Option[PlayerColor] = GameLogic.gameOver(state)
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
  def simulate(red: Player, black: Player) = {
    // TODO use outcome
//    Future {
      val sim = new Simulation(red, black)
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
//    }
  }
}

