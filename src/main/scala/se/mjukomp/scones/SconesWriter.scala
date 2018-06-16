package se.mjukomp.scones

case class SconesWriter() {

  def write(scones: List[Scone]): String =
    scones.map({
      case Leaf(text) => text
      case _          => Stream.empty[Char]
    }).mkString("", "\n", "\n")

}
