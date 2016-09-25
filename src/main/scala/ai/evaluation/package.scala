package ai

import simulator.interfaces.GameState

package object evaluation {

  type Eval = GameState => Double

}
