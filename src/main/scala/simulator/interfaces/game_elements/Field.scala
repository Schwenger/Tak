package simulator.interfaces.game_elements

case class Field(pos: Position) {

  var content: Option[Token] = None

  /**
    * Stacks the other field's content on top this this field's content.
    * @param other: new top of the stack.
    */
  def +=(other: Field) = {
    assert(other.content.isDefined)
    if(this.content.isDefined)
      this.content = Some(other.content.get :: this.content.get)
    else
      this.content = other.content
  }

}
