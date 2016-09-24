import interfaces.{CLI, Interface}

object Main {

  def main(args: Array[String]): Unit = {
    val interfaceSpec = args(0)
    val interface = interfaceSpec match {
      case "-cli" => new CLI()
      case "-gui" => ???
      case "-api" => ???
    }
    interface.greet()
    interface.play()
    while(interface.rematch)
      interface.play()
    interface.bye()
  }

}
