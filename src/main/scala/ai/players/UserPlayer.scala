package ai.players

import parsing.action.ActionParser
import simulator.interfaces.PlayerColor.PlayerColor
import simulator.interfaces.game_elements.{Action, Position, Result}
import simulator.interfaces.{GameLogic, GameState, Player, PlayerMapping}

import scala.io.StdIn
import scala.util.{Failure, Success}

class UserPlayer(color: PlayerColor) extends Player {
  override val kind: PlayerColor = color

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

  def readAction(state: GameState): Action = ActionParser(StdIn.readLine()) match {
    case Success(a) =>
      if(!GameLogic.isValid(state, a)) {
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
    * @param occupied reports the only already occupied game position if the player is black
    * @return the position for the opponents first Minion
    */
  override def firstAction(occupied: Option[Position]): Position = ???

  /**
    * Called once after the game is decided.
    * Contains information about the winner for self-evaluation.
    *
    * @param result information about the winner and the final state.
    */
  override def accept(result: Result): Unit = ???
}
