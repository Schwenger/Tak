package simulator.logic

import simulator.interfaces.PlayerColor.{Black, PlayerColor, Red}
import simulator.interfaces.elements.Direction.Direction
import simulator.interfaces.elements.{Direction, Position, Token}
import simulator.interfaces.{GameState, PlayerMapping}

object GameOver {

  /**
    * Decides whether a given State is terminal. Returns the winning player, if there is one.
    * @param state for which this method decides whether it is terminal.
    * @return The winning player, if any.
    */
  def apply(state: GameState) : Option[PlayerColor] = {
    if(state.surrendered.isDefined)
      return state.surrendered
    val horizontal = hasStreet_impl1(state, Direction.Right, i => Position(0, i))
    val vertical = hasStreet_impl1(state, Direction.Up, i => Position(i, 0))
    (horizontal.red || vertical.red, horizontal.black || vertical.black) match {
      case (true, false) => Some(Red)
      case (false, true) => Some(Black)
      case (true, true) => throw new IllegalStateException("This case must not occur.")
      case (false, false) => fullBoard(state)
    }
  }

  def fullBoard(state: GameState): Option[PlayerColor] = {
    if(state.freeFields != 0)
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

  def hasStreet_impl1(state: GameState, dir: Direction, initial: Int => Position): PlayerMapping[Boolean] = {
    val (orth1, orth2) = dir.orth

    def qualifyingPos(pos: Position)(implicit color: PlayerColor): Boolean =
      pos.valid(state.size) && state.dominatedBy(pos, color) && state(pos).exists(_.streetable)

    def qualifyingField(field: Option[Token], color: PlayerColor): Boolean =
      field exists (token => token.player == color && token.streetable)

    def nextStep(open: Seq[Position])(implicit color: PlayerColor): Seq[Position] =
      open collect { case pos if qualifyingField(state(dir(pos)), color) => dir(pos) }

    def expand(open: Seq[Position])(implicit color: PlayerColor): Seq[Position] = {
      def move(pos: Position, dir: Direction): Seq[Position] = {
        if (qualifyingPos(pos))
          move(dir(pos), dir) :+ pos
        else
          Nil
      }
      open.foldLeft(Nil: Seq[Position]) { (accu: Seq[Position], pos: Position) =>
        val move1 = move(pos, orth1)
        val move2 = move(pos, orth2)
        move1 ++ move2 ++ accu
      }.distinct
    }
    def checkBoard(implicit color: PlayerColor) = {
      var open: Seq[Position] = for {i <- 0 until state.size
                                     if state.dominatedBy(initial(i), color)} yield initial(i)
      for(i <- 1 until state.size){
        open = expand(nextStep(open))
      }
      open
    }
    val red = checkBoard(Red)
    val black = checkBoard(Black)
    PlayerMapping(red.nonEmpty, black.nonEmpty)
  }

}
