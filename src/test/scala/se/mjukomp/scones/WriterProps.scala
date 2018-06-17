package se.mjukomp.scones

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import se.mjukomp.scones.Scone._

object WriterProps extends Properties("SconesWriter") {

  val writer = SconesWriter()

  def write(scones: Scone*): String =
    writer.write(scones.toList)

  property("EmptyData") =
    write() ?= "\n"

  property("OneLeaf") =
    write(Leaf("OneLeaf")) ?= "OneLeaf\n"

  property("TwoLeafs") =
    write(Leaf("One"), Leaf("Two")) ?=
      """|One
         |Two
         |""".stripMargin

  property("OneGroup") =
    write(group("One", "Two", "Three")) ?=
      """|(One
         |    Two
         |    Three)
         |""".stripMargin

  property("EmptyGroup") =
    write(group()) ?=
      """|()
         |""".stripMargin

  property("NestedGroup") =
    write(group("Alfa", group("Beta", group("Gamma"), "Delta"))) ?=
      """|(Alfa
         |    (Beta
         |        (Gamma)
         |        Delta))
         |""".stripMargin

  property("QuoteLeftParentesis") =
    write("NoQuoting", "LeafWith(InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith(InIt"
         |NoQuoting
         |""".stripMargin

  property("QuoteRightParentesis") =
    write("NoQuoting", "LeafWith)InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith)InIt"
         |NoQuoting
         |""".stripMargin

  property("EscapeQuoteInQuotes") =
    write("NoQuoting", "LeafWith)And\"InIt", "NoQuoting") ?=
      """|NoQuoting
         |"LeafWith)And\"InIt"
         |NoQuoting
         |""".stripMargin
}
