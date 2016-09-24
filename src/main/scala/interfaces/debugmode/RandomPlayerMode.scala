package interfaces.debugmode

import ai.players.RandomPlayer
import simulator.interfaces.{PlayerColor, Simulator}

object RandomPlayerMode {

  def apply(args: Seq[String]): Unit = {
    var size = 4
    var runs = 1
    for(i <- args.indices by 2) {
      args(i) match {
        case "-size" | "-s" => size = args(i+1).toInt
        case "-runs" | "-r" => runs = args(i+1).toInt
      }
    }

    println(s"Config: runs: $runs (dft: 1); size: $size (dft: 4)")

    println("Instantiating RandomPlayer.")
    val red = new RandomPlayer(PlayerColor.Red, size)
    val black = new RandomPlayer(PlayerColor.Black, size)

    val start = System.currentTimeMillis()
    for(i <- 0 until runs) {
      println(s"Starting Simulation $i.")
      Simulator(red, black, size)
    }
    val stop = System.currentTimeMillis()

    println(s"Simulation terminated successfully. Time elapsed for $runs simulations: ${stop - start}")
    println(s"Red won: ${Simulator.won._1}, Black won: ${Simulator.won._2}.")

  }

}
