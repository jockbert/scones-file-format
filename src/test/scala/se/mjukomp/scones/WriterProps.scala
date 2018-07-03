package se.mjukomp.scones

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import se.mjukomp.scones.Scone._

object WriterProps extends Properties("SconesWriter") {

  val writer = SconesWriter()

  def write(scones: Scone*): String =
    writer.write(scones.toList)

  property("Empty data") =
    write() ?= "\n"

  property("One lLeaf") =
    write(Leaf("OneLeaf")) ?= "OneLeaf\n"

  property("Two leafs") =
    write(Leaf("One"), Leaf("Two")) ?=
      """|One
         |Two
         |""".stripMargin

  property("One group") =
    write(group("One", "Two", "Three")) ?=
      """|(One
         |    Two
         |    Three)
         |""".stripMargin

  property("Empty group") =
    write(group()) ?=
      """|()
         |""".stripMargin

  property("Nested groups") =
    write(group("Alfa", group("Beta", group("Gamma"), "Delta"))) ?=
      """|(Alfa
         |    (Beta
         |        (Gamma)
         |        Delta))
         |""".stripMargin

  property("Quote whitespace") =
    write("NoQuoting", "a\tb", "a\nb", "a\rb", "a b", "NoQuoting") ?=
      "NoQuoting\n\"a\tb\"\n\"a\nb\"\n\"a\rb\"\n\"a b\"\nNoQuoting\n"

  property("Quote left parentesis") =
    write("NoQuoting", "LeafWith(InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith(InIt"
         |NoQuoting
         |""".stripMargin

  property("Quote right parentesis") =
    write("NoQuoting", "LeafWith)InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith)InIt"
         |NoQuoting
         |""".stripMargin

  property("Escape quote if in quotes") =
    write("NoQuoting", "LeafWith)\"InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith)\"InIt"
         |NoQuoting
         |""".stripMargin

  // --- Properties below detected by roundtrip test -------

  property("QuoteZeroLengthText") =
    write("", "", "") ?=
      """|""
         |""
         |""
         |""".stripMargin

  property("SingleQuote") =
    write("a\"b") ?=
      """|"a\"b"
         |""".stripMargin

  property("TrailingBackSlashInQuote") =
    write(" \\") ?=
      """|" \\"
         |""".stripMargin
}
