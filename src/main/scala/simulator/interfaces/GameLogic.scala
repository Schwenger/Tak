package simulator.interfaces

import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements._
import simulator.interfaces.game_elements.ActionKind.ActionKind
import simulator.interfaces.game_elements.Direction.Direction

object GameLogic {

  /**
    * Decides whether a given State is terminal. Returns the winning player, if there is one.
    * @param state for which this method decides whether it is terminal.
    * @return The winning player, if any.
    */
  def gameOver(state: GameState) : Option[PlayerColor] = None

  /**
    * Collects the Actions a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def availableActions(state: GameState)(implicit color: PlayerColor): List[Action] = Nil

  /**
    * Collects the Actions of a given Kind a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param actionKind the kind of Actions which are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def availableActions(state: GameState, actionKind: ActionKind)(implicit color: PlayerColor): List[Action] = Nil

  /**
    * Decides whether a given Action is applicable in a given State for a given Player.
    * @param state in which the action might be applicable
    * @param action which is checked for being applicable.
    * @param color of the player.
    * @return
    */
  def isValid(state: GameState, action: Action)(implicit color: PlayerColor): Boolean = action match {
    case PlaceCapstone(dest) => validPos(state.size ,dest) && state(dest).isEmpty && state.capstonesLeft(color) > 0
    case PlaceMinion(dest, _) => validPos(state.size, dest) && state(dest).isEmpty && state.minionsLeft(color) > 0
    case PlaceWall(dest) => validPos(state.size, dest) && state(dest).isEmpty && state.minionsLeft(color) > 0
    case Slide(src, stones, dir) => validSlide(state, src, stones, dir)
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
    if(!validPos(state.size, src))
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
      if(!validPos(state.size, dest) || state(dest).isDefined && !_stackable(top, state(dest).get))
        return false
      inHand = inHand.take(n)
      dest = dir(dest)
    }
    if(!validPos(state.size, dest) || state(dest).isDefined && !_stackable(Tokenizer(inHand).get, state(dest).get))
      return false

    true

  }

  @inline private def _stackable(top: Token, bot: Token): Boolean = !bot.blocking || top.crushing && bot.crushable
  @inline private def validPos(bound: Int, pos: Position) = (0 until bound contains pos.x) && (0 until bound contains pos.y)


}
