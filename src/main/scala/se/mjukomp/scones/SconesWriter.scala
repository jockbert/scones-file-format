package se.mjukomp.scones

case class SconesWriter() {

  private def escapeQuotes(text: String): String =
    text.replace("\"", "\\\"")

  private def escapeBackslash(text: String): String =
    text.replace("\\", "\\\\")

  private def quoteIfNeeded(text: String): String =
    if (text.contains('(')
      || text.contains(')')
      || text.isEmpty()
      || text.contains('"')
      || !text.forall(!Scone.isWhitespace(_)))
      "\"" + escapeQuotes(escapeBackslash(text)) + "\""
    else text

  def write(
    scones: List[Scone],
    indent: Int         = 0): String = {

    val padding = "    " * indent
    scones.map({
      case Leaf(text) => quoteIfNeeded(text)
      case Group(sub) => "(" + write(sub, indent + 1).trim() + ")"
    }).mkString(padding, "\n" + padding, "\n")
  }

  def write(scones: Scone*): String =
    write(scones.toList)

  def write(scone: Scone): String =
    write(scone :: Nil)
}
