package simulator.logic

import simulator.interfaces.GameState
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Direction.Direction
import simulator.interfaces.elements._

object ActionValidator {

  /**
    * Decides whether a given Action is applicable in a given State for a given Player.
    * @param state in which the action might be applicable
    * @param action which is checked for being applicable.
    * @param color of the player.
    * @return
    */
  def apply(state: GameState, action: Action)(implicit color: PlayerColor): Boolean = {
    if(!action.origin.valid(state.size))
      return false

    action match {
      case PlaceCapstone(dest) => state(dest).isEmpty && state.capstonesLeft(color) > 0
      case PlaceMinion(dest) => state(dest).isEmpty && state.minionsLeft(color) > 0
      case PlaceWall(dest) => state(dest).isEmpty && state.minionsLeft(color) > 0
      case Slide(src, stones, dir) => validSlide(state, src, stones, dir)
      case Move(src, dir) => validMove(state, src, dir)
      case Surrender(_) => true
    }
  }

  private def validMove(state: GameState, src: Position, dir: Direction)(implicit player: PlayerColor): Boolean = {
    if(!state.dominatedBy(src, player))
      return false
    val dest = dir(src)
    dir(src).valid(state.size) && (state(dest).isEmpty || _stackable(state(src).get, state(dest).get))
  }

  // TODO iterative -> recursive
  private def validSlide(state: GameState, src: Position, stones: List[Int], dir: Direction)(implicit player: PlayerColor): Boolean = {
    if(!state.dominatedBy(src, player))
      return false
    if(stones.head > state.size)
      return false
    if(!(stones == stones.sortBy(-_).distinct)) // checks for strictly monotonic decreasing
      return false
    if(stones.min <= 0)
      return false

    var inHand = state(src).get match {
      case Stack(content) => content
      case x => List(x)
    }
    inHand = inHand.take(stones.head)

    var dest = dir(src)
    if(inHand.length < stones.head)
      return false
    for(n <- stones.tail) {
      val top = Tokenizer(inHand.drop(n)).get
      if(!dest.valid(state.size) || state(dest).isDefined && !_stackable(top, state(dest).get))
        return false
      inHand = inHand.take(n)
      dest = dir(dest)
    }
    if(!dest.valid(state.size) || state(dest).isDefined && !_stackable(Tokenizer(inHand).get, state(dest).get))
      return false

    true

  }

  @inline private def _stackable(top: Token, bot: Token): Boolean = !bot.blocking || top.crushing && bot.crushable

}
