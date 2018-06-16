package se.mjukomp.scones

case class SconesWriter() {
  def write(scones: List[Scone]): Stream[Char] =
    scones.toStream.flatMap({
      case Leaf(text) => text.toStream
      case _          => Stream.empty[Char]
    })

}
