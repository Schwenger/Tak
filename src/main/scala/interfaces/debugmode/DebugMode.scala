package interfaces.debugmode

import simulator.interfaces.{Player, Simulator}

abstract class DebugMode {

  protected def apply(size: Int, runs: Int, red: Player, black: Player) = {

    println(s"Config: runs: $runs (dft: 1); size: $size (dft: 4)")
    println("Instantiating RandomPlayer.")

    val start = System.currentTimeMillis()
    for(i <- 0 until runs) {
      println(s"Starting Simulation $i.")
      Simulator(red, black, size)
    }
    val stop = System.currentTimeMillis()
    val elapsed = stop - start

    println("Simulation terminated successfully.")
    println(s"Time elapsed for $runs simulations: $elapsed. That's ${elapsed / runs} per run.")
    println(s"Red won: ${Simulator.won._1}, Black won: ${Simulator.won._2}.")
  }

  protected def parseCommandLine(args: Seq[String]): (Int, Int) = {
    var size = 4
    var runs = 1
    for(i <- args.indices by 2) {
      args(i) match {
        case "-size" | "-s" => size = args(i+1).toInt
        case "-runs" | "-r" => runs = args(i+1).toInt
      }
    }
    (size, runs)
  }

}
