package ai.players

import parsing.action.ActionParser
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Action, PlaceMinion, Position, Result}
import simulator.interfaces.{GameLogic, GameState, Player, PlayerMapping}

import scala.io.StdIn
import scala.util.{Failure, Success}

class UserPlayer(override val kind: PlayerColor, override val boardSize: Int) extends Player {

  /**
    * Called once before the actual game starts.
    *
    * @param state initial state including all game parameters like board size.
    */
  override def init(state: GameState): Unit = {
    println("Welcome to a round of Tak. Prepare to be obliterated.")
    println()
  }

  /**
    * Called multiple times during one game.
    * Computes the next action the player deems most sensible by their own metrics.
    *
    * @param turn  this player's and the opponent's last action
    * @param state the current game state
    * @return the action that is supposed to be executed next for this player.
    */
  override def nextAction(turn: PlayerMapping[Action], state: GameState): Action = {
    println("The opposing player used " + turn.black)
    println()
    println(state)
    println()
    println("What do you want to do?")
    readAction(state)
  }

  def readAction(state: GameState, first: Boolean = false): Action = ActionParser(StdIn.readLine()) match {
    case Success(a) =>
      val valid = GameLogic.isValid(state, a)(kind)
      lazy val validFirst = a.isInstanceOf[PlaceMinion]
      if (!valid || first && !validFirst) {
        println("This was an invalid move. Either you are trying to cheat and got caught or you are not smart enough for Tak.")
        println("Anyway, try again.")
        readAction(state)
      } else a
    case Failure(e) =>
      println(e)
      readAction(state)
  }


  /**
    * Called one after init and before the first call to nextAction.
    * Computes the position where the opponents first Minion should be placed. This corresponds to the first action.
    *
    * @param state either an empty board or a board with a token of this player is already placed
    * @return the position for the opponents first Minion
    */
  override def firstAction(state: GameState): Position = {
    println("First move: Where do you want to place your opponents token?")
    println()
    println(state)
    println()
    readAction(state, first = true).origin
  }

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    *
    * @param result information about the winner and the final state.
    */
  override def accept(result: Result): Unit = {
    println("Game Over!")
    println("The winner is:")
    println("*drum roll*")
    println()
    println(result.winner)
    val state = result.state
    state.surrendered match {
      case Some(c) => println("This is because the opponent cowardly surrendered.")
      case None => println("You won fair and square in a game to the bitter end.")
    }
    println("The final state was:")
    println()
    println(state.toString)
    println()
    println("I hope you enjoyed the game!")
    println("Thank you, come again! ~ Apu Nahasapeemapetilon")
    println()
  }

}
