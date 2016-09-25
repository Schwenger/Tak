package simulator.elements

import simulator.interfaces.PlayerColor.PlayerColor

// Walls and Capstones are blocking.
// Everything but Capstones is crushable
// Capstones are crushing
// For Stacks the topmost token counts except in terms of crushing. A stack can never be crushing
// Only Minions count as a point
// Everything but walls counts for streets
sealed abstract class Token(val player: PlayerColor, val blocking: Boolean, val crushing: Boolean, val crushable: Boolean, val streetable: Boolean, val worthAPoint: Boolean) {
  /**
    * Stacks the other Token's content on top of this Token's content. Creates a Stack in the process.
    * The own content might be modified in case it is topped by a Wall and crushed by a Capstone.
    * The Capstone can potentially be part of a Stack.
    * @param top: token to be stacked on top of this Token.
    * @return
    */
  def ::(top: Token): Token
}

object Tokenizer {
  def apply(list: List[Token]): Option[Token] = list match {
    case Nil => None
    case List(x) => Some(x)
    case _ => Some(Stack(list))
  }
}

case class Wall(p: PlayerColor) extends Token(p, blocking = true, crushing = false, crushable = true,
  streetable = false, worthAPoint = false) {

  override def ::(top: Token): Token = {
    assert(top.crushing)
    val crushed = Minion(player)
    top match {
      case Stack(content) => Stack(content :+ crushed)
      case Capstone(_) => Stack(List(top, crushed))
      case _ => throw new UnsupportedOperationException("This is not allowed at all.")
    }
  }
}

case class Minion(p: PlayerColor) extends Token(player = p, blocking = false, crushing = false, crushable = true,
  streetable = true, worthAPoint = true) {

  override def ::(top: Token): Token = {
    top match {
      case Stack(content) => Stack(content ++ List(this))
      case _ => Stack(List(top, this))
    }
  }
}

case class Stack(content: List[Token]) extends Token(player = content.head.player, blocking = content.head.blocking,
  crushing = false, crushable = content.head.crushable, streetable = content.head.streetable,
  worthAPoint = content.head.worthAPoint) {

  assert(!content.tail.exists(t => t.isInstanceOf[Capstone] || t.isInstanceOf[Wall]))

  override def ::(top: Token): Token = {
    assert(!this.blocking || this.crushable && top.crushing)
    var botList = this.content
    if(this.blocking) {
      botList = Minion(this.content.head.player) :: this.content.tail
    }
    top match {
      case Stack(otherContent) => Stack(otherContent ::: botList)
      case _ => Stack(top :: botList)
    }
  }
}

case class Capstone(p: PlayerColor) extends Token(player = p, blocking = true, crushing = true, crushable = false,
  streetable = true, worthAPoint = false) {

  override def ::(top: Token): Token = {
    throw new IllegalArgumentException // nothing can go on top of a Capstone
  }
}
