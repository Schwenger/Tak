package interfaces.debugmode

import ai.evaluation. TokenCount
import ai.players.MinMaxPlayer
import simulator.interfaces.{Player, PlayerColor}

object MinMaxPlayerMode extends DebugMode {

  def apply(args: Seq[String]): Unit = {

    val (size, runs) = super.parseCommandLine(args)
    val red = new MinMaxPlayer(PlayerColor.Red, new TokenCount(PlayerColor.Red), depth = 3, boardSize = size)
    val black = new MinMaxPlayer(PlayerColor.Black, new TokenCount(PlayerColor.Black), depth = 4, boardSize = size)

    super.apply(size, runs, red, black)

  }

}
