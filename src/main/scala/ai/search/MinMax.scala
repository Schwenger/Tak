package ai.search
import ai.evaluation.Evaluator
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.elements.Action
import simulator.interfaces.{ActionExecutor, GameState}

sealed trait Comp {
  def apply[R](l: Seq[R])(implicit order: Ordering[R]): R
  def unary_!(): Comp
}
object Min extends Comp {
  def apply[R](l: Seq[R])(implicit order: Ordering[R]) = l.min(order)
  def unary_!() = Max
}
object Max extends Comp {
  def apply[R](l: Seq[R])(implicit order: Ordering[R]) = l.max(order)
  def unary_!() = Min
}

object MinMax extends SearchStrategy {

  class MMNode(val state: GameState, val action: Action, val context: Int)
    extends SearchNode[Int](state, action, context)

  /**
    * Applies the MinMax search strategy on the given state.
    * Returns the best action with respect to maximizing the eval function.
    *
    * @param state the state for which the best action is supposed to be computed
    * @param eval estimates the quality of a state. Should always result positive values.
    * @param actionSupplier supplies applicable actions for a given state and color.
    * @param depth depth 1 means only the immediately reachable states are taken into account
    * @param maxPlayer the player whose turn it is
    * @return best state w.r.t maximizing eval
    */
  override def apply(state: GameState, eval: Evaluator, actionSupplier: (GameState) => Seq[Action],
                     depth: Int, maxPlayer: PlayerColor): Action = {

    implicit val order = Ordering.by[MMNode, Int](_.context)
    assert(depth > 0)
    /*
    Recursion step; calls itself until depth becomes 0.
    States are then evaluated and the maximizing/minimizing nodes gets passed upwards
    such that the best action w.r.t. eval for maxPlayer can be returned.
     */
    def run(state: GameState, action: Action, depth: Int, best: Comp, color: PlayerColor): MMNode = {
      assert(depth > 0 || action != null)
      if(depth == 0)
        new MMNode(state, action, eval(color, state))
      else {
        def expandState(state: GameState, action: Action): GameState = ActionExecutor(action, state, color)
        def expandAction(action: Action) = run(expandState(state, action), action, depth - 1, !best, !color)
        val a = actionSupplier(state)
        val expanded: Seq[MMNode] = actionSupplier(state) map expandAction
        best(expanded)
      }
    }
    run(state, null, depth, Max, maxPlayer).action
  }
}
