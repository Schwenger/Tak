package interfaces

import simulator.interfaces.{Player, PlayerMapping}

trait Interface {

  private var players: PlayerMapping[Player] = _

  protected def setPlayers(red: Player, black: Player) = {
    assert(players == null)
    players = PlayerMapping(red, black)
  }

  def greet(): Unit
  def play(): Unit
  def rematch: Boolean
  def summary(): Unit
  def bye(): Unit

}
