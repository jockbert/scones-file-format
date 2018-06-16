package se.mjukomp.scones

case class SconesWriter() {

  def write(
    scones: List[Scone],
    indent: Int         = 0): String = {

    val padding = "    " * indent

    scones.map({
      case Leaf(text) => text
      case Group(sub) => "(" + write(sub, indent + 1).trim() + ")"
    }).mkString(padding, "\n" + padding, "\n")
  }
}
