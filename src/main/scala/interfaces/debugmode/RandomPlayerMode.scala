package interfaces.debugmode

import ai.evaluation.Domination
import ai.players.RandomPlayer
import simulator.interfaces.PlayerColor

object RandomPlayerMode extends DebugMode {

  def apply(args: Seq[String]) = {
    val (size, runs) = super.parseCommandLine(args)
    val red = new RandomPlayer(PlayerColor.Red, size)
    val black = new RandomPlayer(PlayerColor.Black, size)

    super.apply(size, runs, red, black)

  }

}
