package parsing.action

import simulator.interfaces.game_elements.Direction.Direction
import simulator.interfaces.game_elements._

import scala.util.{Failure, Success, Try}

object NeededHelp extends Throwable
case class ParseException(msg: String) extends Throwable {
  override def toString: String = super.toString + msg
}

object ActionParser {

  def apply(str: String): Try[Action] = {
    val instr = str.toLowerCase split " " filter (_.length > 0)
    instr(0) match {
      case "move" => parseMove(instr.slice(1, instr.length))
      case "slide" => parseSlide(instr.slice(1, instr.length))
      case "place" => parsePlace(instr.slice(1, instr.length))
      case "-h" => printHelp(); Failure(NeededHelp)
    }
  }

  private def kind2Constructor(kind: String): Position => Action = kind match {
    case "capstone" => PlaceCapstone
    case "minion" | "flat" | "flatstone" => PlaceMinion(_, pseudo = false)
    case "wall" | "standing" | "standingstone" => PlaceWall
  }

  private def parsePlace(strs: Seq[String]): Try[Action] = {
    var stack = strs
    if(stack.head == "a")
      stack = stack.tail
    val kind = stack.head
    var remaining = stack.tail
    var offset = 1
    if(remaining.head == "at") {
      remaining = remaining.tail
      offset += 1
    }
    parsePosition(remaining) match {
      case Success((pos, off)) =>
        if(remaining.length > off)
          Failure(tooLong(offset + remaining.length - off))
        else
          Success(kind2Constructor(kind)(pos))
      case Failure(e) => Failure(e)
    }
  }

  private def printHelp(): Unit = println("Ain't nobody got time for coming up with a help text.")

  private def tooLong(index: Int): ParseException =
    ParseException(s"Your instruction is too long. The first $index components were sufficient")

  private def parseMove(instr: Seq[String]): Try[Slide] = {
    var ix = 0
    if(instr(ix) == "from ")
      ix += 1
    parsePosition(instr.slice(ix, instr.length)) match {
      case Success((src, offset)) =>
        ix += offset
        val dir = parseDirection(instr(ix))
        ix += 1
        if (ix != instr.length)
          Failure(tooLong(ix + 1))
        else
          dir.map(Slide(src, List(1), _))
      case Failure(e) => Failure(e)
    }
  }

  private def parseSlide(instr: Seq[String]): Try[Slide] = {
    var ix = 0
    if(instr(ix) == "from ")
      ix += 1
    parsePosition(instr.slice(ix, instr.length)) match {
      case Success((src, offset)) =>
        ix += offset
        val dir = parseDirection(instr(ix))
        ix += 1
        if (ix != instr.length)
          Failure(tooLong(ix + 1))
        else
          dir.map(Slide(src, List(1), _))
      case Failure(e) => Failure(e)
    }
  }

  private def parseDirection(str: String): Try[Direction] = str match {
    case "north" | "up" | "top" => Success(Direction.Up)
    case "south" | "down" | "bot" => Success(Direction.Down)
    case "east" | "right" => Success(Direction.Right)
    case "west" | "left" => Success(Direction.Left)
    case _ => Failure(ParseException("Unknown word. Expected direction, but got: " + str))
  }

  private def s2i(str: String): Option[Int] = try {
    Some(str.toInt)
  } catch {
    case e: NumberFormatException => None
  }

  private def parsePosition(strs: Seq[String]): Try[(Position, Int)] = {
    val full = strs.mkString(" ")
    println(full)
    val long = "Position(%w*%d%w*,%w*%d%w*)"
    val short = "(%w*%d%w*,%w*%d%w*)"
    println(full.matches(long))
    println(full.matches(short))
    val prefixLength = if(full.matches(long)) "Position(".length else if (full.matches(short)) "(".length else -1
    if(prefixLength == -1)
      return Failure(ParseException(s"Expected Position but got: $full}. \n Please use e.g. (3,2) or Position(3, 2)."))
    val remaining: Seq[String] = full.slice(prefixLength, full.length) split " " flatMap (_.split(",")) filter (_.length > 0)
    if(remaining.length < 2)
      return Failure(ParseException(s"You messed up hard. Try again."))
    val significant: Seq[Option[Int]] = remaining slice (0, 2) map s2i // we expect two numbers
    val (x, y) = (significant.head, significant.tail.head)
    if(significant exists (_.isEmpty))
      return Failure(ParseException(s"Cannot parse x and/or y coordinate. Please use e.g. (3,2) or Position(3, 2)."))

    val offset = strs.indexWhere(str => str.contains(")")) + 1
    Success(Position(x.get, y.get), offset)
  }

}
