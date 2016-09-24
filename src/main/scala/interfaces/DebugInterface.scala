package interfaces

import interfaces.debugmode.RandomPlayerMode

class DebugInterface(args: Seq[String]) extends Interface {

  assert(args.head == "-mode" || args.head == "-m")
  args(1).toLowerCase match {
    case "randomplayermode" => RandomPlayerMode(args.slice(2, args.length))
    case _ => assert(false)
  }

  override def greet(): Unit = ()

  override def play(): Unit = ()

  override def rematch: Boolean = false

  override def summary(): Unit = ()

  override def bye(): Unit = ()
}
