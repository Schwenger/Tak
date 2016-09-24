import interfaces.{CLI, DebugInterface}

object Main {

  def main(args: Array[String]): Unit = {
    val interfaceSpec = args(0)
    val interface = interfaceSpec match {
      case "-cli" => new CLI()
      case "-gui" => ???
      case "-api" => ???
      case "-debug" => new DebugInterface(args.slice(1, args.length))
    }
    interface.greet()
    interface.play()
    while(interface.rematch)
      interface.play()
    interface.bye()
  }

}
