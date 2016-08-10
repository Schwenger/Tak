package simulator.interfaces.game_elements

import simulator.interfaces.PlayerColor.PlayerColor

// Walls and Capstones are blocking.
// Everything but Capstones is crushable
// Capstones are crushing
// For Stacks the topmost token counts.
sealed abstract class Token(val player: PlayerColor, val blocking: Boolean, val crushing: Boolean, val crushable: Boolean) {
  /**
    * Stacks the other Token's content on top of this Token's content. Creates a Stack in the process.
    * The own content might be modified in case it is topped by a Wall and crushed by a Capstone.
    * The Capstone can potentially be part of a Stack.
    * @param other: token to be stacked on top of this Token.
    * @return
    */
  def ::(other: Token): Token
}

case class Wall(p: PlayerColor) extends Token(p, blocking = true, crushing = false, crushable = true) {
  override def ::(other: Token): Token = {
    assert(other.crushing)
    val crushed = Minion(player)
    other match {
      case Stack(content) => Stack(crushed :: content)
      case Capstone(_) => Stack(List(crushed, other))
    }
  }
}

case class Minion(p: PlayerColor) extends Token(player = p, blocking = false, crushing = false, crushable = true) {
  override def ::(other: Token): Token = {
    assert(!other.blocking)
    other match {
      case Stack(content) => Stack(this :: content)
      case _ =>
    }
  }
}

case class Stack(content: List[Token]) extends Token(player = content.head.player, blocking = content.head.blocking, crushing = content.head.crushing, crushable = content.head.crushable) {
  assert(!content.tail.exists(t => t.isInstanceOf[Capstone] || t.isInstanceOf[Wall]))

  override def ::(other: Token): Token = {
    assert(!this.blocking || this.crushable && other.crushing)
    var top = this.content
    if(this.blocking) {
      top = Minion(this.content.head.player) :: this.content.tail
    }
    other match {
      case Stack(otherContent) => Stack(top ::: otherContent)
      case _ => Stack(top :+ other)
    }
  }
}

case class Capstone(p: PlayerColor) extends Token(player = p, blocking = true, crushing = true, crushable = false) {
  override def ::(other: Token): Token = {
    throw new IllegalArgumentException // nothing can go on top of a Capstone
  }
}
