package parsing.state

import simulator.{GameState, PlayerColor}
import simulator.elements._

import scala.annotation.tailrec

object StateDeserializer {

  def apply(str: String): GameState = {
    val strs = str.split('\n').reverse map (_.trim()) filter (_.length > 0)
    val state = new GameState(strs.length)
    val consume = (row: Int) => (col: Int, t: Option[Token]) => state.setField(Position(x = col, y = row), t)
    deserializeLines(consume, strs.toList, 0)
    state
  }

  @tailrec private def deserializeLines(consume: (Int) => (Int, Option[Token]) => Unit, lines: Seq[String], row: Int): Unit =
    lines match {
      case Nil => ()
      case line :: rest =>
        line2tok(line, consume(row))
        deserializeLines(consume, rest, row + 1)
    }

  private def line2tok(line: String, consume: (Int, Option[Token]) => Unit): Unit = {
    val tokens = line.split("&") filter (_.length > 0) map (_.trim()) map str2tok
    tokens.zipWithIndex foreach {t => consume.tupled(t.swap)}
  }

  private def str2tok(t: String): Option[Token] = {
    def color (id: Char) = id match {
      case 'R' => PlayerColor.Red
      case 'B' => PlayerColor.Black
    }
    def single(str: String) = {
      assert(str.length == 2)
      val c = color(str.head)
      str(1) match {
        case 'M' => Minion(c)
        case 'W' => Wall(c)
        case 'C' => Capstone(c)
      }
    }
    if(t.length > 2) {
      assert(t.startsWith("Stack(") && t.endsWith(")"))
      val content = t.substring("Stack(".length, t.length - 1) split "," map (_.trim()) map single
      Some(Stack(content.toList))
    } else if(t.length == 1) {
      assert(t.equals("_"))
      None
    } else {
      Some(single(t))
    }
  }

}
