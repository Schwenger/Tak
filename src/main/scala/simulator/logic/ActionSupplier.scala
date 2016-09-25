package simulator.logic

import simulator.GameState
import simulator.PlayerColor.PlayerColor
import simulator.elements.ActionKind.ActionKind
import simulator.elements.Direction.Direction
import simulator.elements._

object ActionSupplier {

  /**
    * Collects the Actions a given Player can apply in the given State. Returns an empty list if there is none.
    * @note does *not* return `Surrender`.
    * @param state for which the applicable Actions are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def apply(state: GameState)(implicit color: PlayerColor): Seq[Action] = {

    val placeActions = places(state, color)
    val moveActions = moves(state, color, moves = true, slides = true)

    val res: Seq[Action] = placeActions ++ moveActions

    assert(res forall (ActionValidator(state, _)))

    res
  }

  /**
    * Collects the Actions of a given Kind a given Player can apply in the given State. Returns an empty list if there is none.
    * @param state for which the applicable Actions are collected.
    * @param actionKind the kind of Actions which are collected.
    * @param color of the player for which Actions are collected.
    * @return list of applicable actions
    */
  def apply(state: GameState, actionKind: ActionKind)(implicit color: PlayerColor): Seq[Action] = actionKind match {
    case ActionKind.Move => moves(state, color, moves = true, slides = false)
    case ActionKind.Place => places(state, color)
    case ActionKind.Slide => moves(state, color, moves = false, slides = true)
    case ActionKind.Surrender => Seq(Surrender())
  }

  private def moves(state: GameState, color: PlayerColor, moves: Boolean, slides: Boolean): Seq[Action] = {
    Direction.all flatMap { dir =>
      dominatedFields(state)(color) flatMap { pos =>
        token2move(state(pos).get, pos, dir, moves = moves, slides = slides)
      }
    } filter (ActionValidator(state, _)(color))
  }

  private def places(state: GameState, color: PlayerColor) = {
    val empty = emptyFields(state)

    val placeMin: Seq[Action] = if (state.minionsLeft(color) > 0) empty map PlaceMinion else Nil
    val placeWall: Seq[Action] = if (state.minionsLeft(color) > 0) empty map PlaceWall else Nil
    val placeCap: Seq[Action] = if (state.capstonesLeft(color) > 0) empty map PlaceCapstone else Nil

    placeMin ++ placeWall ++ placeCap
  }

  private def token2move(token: Token, pos: Position, dir: Direction, moves: Boolean, slides: Boolean): List[Action] = token match {
    case Stack(content) => if(slides) makeSlide(content.size, pos, dir) else Nil
    case _ => if(moves) List(Move(pos, dir)) else Nil
  }

  private def makeSlide(stackSize: Int, pos: Position, dir: Direction): List[Slide] = {
    def _dropSet(n: Int): List[List[Int]] = n match {
      case 0 => List(Nil)
      case _ =>
        val rec = _dropSet(n - 1)
        rec ::: (rec map (n :: _))
    }
    _dropSet(stackSize) filter(_.nonEmpty) map (seq => Slide(pos, seq, dir))
  }

  @inline def emptyFields(state: GameState): Seq[Position] = state.zipWithPosition.flatten filter (_._1.isEmpty) map (_._2)

  @inline def dominatedFields(state: GameState)(implicit color: PlayerColor): Seq[Position] =
    state.zipWithPosition.flatten filter (_._1.exists(_.player == color)) map(_._2)

}
