package interfaces

import simulator.{Player, PlayerMapping}

trait Interface {

  private var players: PlayerMapping[Player] = _

  protected def setPlayers(red: Player, black: Player) = {
    assert(players == null)
    players = PlayerMapping(red, black)
  }

  protected def red = {
    assert(players != null)
    players.red
  }
  protected def black = {
    assert(players != null)
    players.black
  }

  def greet(): Unit
  def play(): Unit
  def rematch: Boolean
  def summary(): Unit
  def bye(): Unit

}
