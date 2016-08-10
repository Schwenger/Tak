package simulator.interfaces.game_elements

import simulator.interfaces.game_elements.ActionKind.ActionKind

object ActionKind {
  sealed trait ActionKind
  case object Move extends ActionKind
  case object Slide extends ActionKind
  case object Place extends ActionKind
}

trait Action {
  val kind: ActionKind
}
