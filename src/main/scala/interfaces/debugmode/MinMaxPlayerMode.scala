package interfaces.debugmode

import ai.evaluation._
import ai.players.{MinMaxPlayer, RandomPlayer}
import simulator.PlayerColor._

object MinMaxPlayerMode extends DebugMode {

  def apply(args: Seq[String]): Unit = {

    val (size, runs) = super.parseCommandLine(args)

    val red = new MinMaxPlayer(Red, DefaultInterpolator.respective(5)(Red), depth = 3, boardSize = size)
    val black = new RandomPlayer(Black, size)

    super.apply(size, runs, red, black, "MM(5,4)", "Randy")

  }

}
