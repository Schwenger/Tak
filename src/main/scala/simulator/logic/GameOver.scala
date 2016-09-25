package simulator.logic

import simulator.{GameState, PlayerMapping}
import simulator.analysis.Analyzer
import simulator.elements.Direction.Direction
import simulator.elements.{Direction, Position, Token}
import simulator.PlayerColor.{Black, PlayerColor, Red}

object GameOver {

  /**
    * Decides whether a given State is terminal. Returns the winning player, if there is one.
    * @param state for which this method decides whether it is terminal.
    * @return The winning player, if any.
    */
  def apply(state: GameState) : Option[PlayerColor] = {
    if(state.surrendered.isDefined)
      return state.surrendered
    val hor  = Analyzer.minStreetDist(state, hor = true)  map { (r,b) => (r == 0, b == 0) }
    val vert = Analyzer.minStreetDist(state, hor = false) map { (r,b) => (r == 0, b == 0) }
    (hor.red || vert.red, hor.black || vert.black) match {
      case (true, false) => Some(Red)
      case (false, true) => Some(Black)
      case (true, true) => println("lol"); Some(Red)
      case (false, false) => fullBoard(state)
    }
  }

  def fullBoard(state: GameState): Option[PlayerColor] = {
    if(Analyzer.freeFields(state) != 0)
      return None
    def field2points(accu: PlayerMapping[Int], field: Option[Token]): PlayerMapping[Int] = field match {
      case Some(t) if t.player == Red   && t.worthAPoint => PlayerMapping(accu.red + 1, accu.black)
      case Some(t) if t.player == Black && t.worthAPoint => PlayerMapping(accu.red, accu.black + 1)
      case _ => accu
    }
    val points = state.fold(PlayerMapping(0,0))(field2points)
    Math.signum(points.red.compare(points.black)) match {
      case  0 => Some(Black) // we play by the old rules.
      case  1 => Some(Red)
      case -1 => Some(Black)
    }
  }

}
