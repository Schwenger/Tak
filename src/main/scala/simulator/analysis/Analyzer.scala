package simulator.analysis

import simulator.{GameState, PlayerMapping}
import simulator.PlayerColor.{Black, PlayerColor, Red}
import simulator.elements.Direction.{Direction, Down, Up}
import simulator.elements.{Direction, Position}

object Analyzer {

  def freeFields(state: GameState): Int = state.map(_.isEmpty).map(_.count(b => b)).sum

  def domination(state: GameState): PlayerMapping[Int] =
    state.fold(PlayerMapping(0,0))((accu, field) =>
      field.map(_.player) match {
        case Some(Red) => PlayerMapping(accu.red + 1, accu.black)
        case Some(Black) => PlayerMapping(accu.red, accu.black + 1)
        case None => accu
      }
    )

  def minStreetDist(state: GameState, hor: Boolean): PlayerMapping[Int] = {
    type OpenList = List[Position]
    def run(state: GameState, color: PlayerColor, dir: Direction, init: Int => Position): Int = {
      val (o1, o2) = dir.orth

      def qualifyingPos(pos: Position): Boolean =
        pos.valid(state.size) && state(pos).exists(_.streetable) && state.dominatedBy(pos, color)

      def getInitialOpenList(color: PlayerColor, init: Int => Position): OpenList = {
        (Range(0, state.size) collect { case i if qualifyingPos(init(i)) => init(i) }).toList
      }

      def step(open: OpenList, dir: Direction): OpenList = {

        def expandForward(pos: Position): Option[Position] = dir(pos) match {
          case next if qualifyingPos(next) => Some(next)
          case _ => None
        }
        def expandOrth(pos: Position): OpenList = {
          def expandInDir(pos: Position, dir: Direction): OpenList = dir(pos) match {
            case next if qualifyingPos(next) => next :: expandInDir(next, dir)
            case _ => Nil
          }
          expandInDir(pos, o1) ::: expandInDir(pos, o2)
        }

        open flatMap expandForward flatMap (next => next :: expandOrth(next))
      }
      val open = getInitialOpenList(color, init)
      val openLists = Range(0, state.size - 1).foldLeft(List(open))((accu, i) => step(accu.head, dir) :: accu)
      openLists.indexWhere(_.nonEmpty)
    }

    val (dir, init): (Direction, Int => Position) = hor match {
      case true => (Direction.Right, i => Position(0, i))
      case false => (Direction.Up, i => Position(i, 0))
    }

    val red = run(state, Red, dir, init)
    val black = run(state, Black, dir, init)
    PlayerMapping(red, black)
  }

}
