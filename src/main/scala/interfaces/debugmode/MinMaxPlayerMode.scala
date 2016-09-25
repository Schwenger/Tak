package interfaces.debugmode

import ai.evaluation.Domination
import ai.players.{MinMaxPlayer, RandomPlayer}
import simulator.interfaces.PlayerColor

object MinMaxPlayerMode extends DebugMode {

  def apply(args: Seq[String]): Unit = {

    val (size, runs) = super.parseCommandLine(args)
    val red = new MinMaxPlayer(PlayerColor.Red, Domination(PlayerColor.Red), depth = 3, boardSize = size)
//    val black = new MinMaxPlayer(PlayerColor.Black, new TokenCount(PlayerColor.Black), depth = 4, boardSize = size)
    val black = new RandomPlayer(PlayerColor.Black, size)

    super.apply(size, runs, red, black)

  }

}
