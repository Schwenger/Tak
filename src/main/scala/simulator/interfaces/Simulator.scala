package simulator.interfaces

import java.util.concurrent.atomic.AtomicInteger

import simulator.model.GameState

import scala.collection.mutable
import scala.concurrent.Future

private class Simulator(red: Player, black: Player) {
  val state = new GameState
  def simulateTurn() = {

  }
  def gameOver: Boolean = GameLogic.gameOver(state)
}

object Simulator {

  val simulations = new mutable.MutableList[Simulator]
  val running = new AtomicInteger(0)
  val over = new AtomicInteger(0)
  val won = (new AtomicInteger(0), new AtomicInteger(0))

  def simulate(red: Player, black: Player) = {
    // TODO: run simulation in new thread.
    Future {
      val sim = new Simulator(red, black)
      do {
        sim.simulateTurn()
      } while (sim.gameOver)
    }
  }
}

