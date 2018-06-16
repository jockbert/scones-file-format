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

  property("OneElement") =
    write(Leaf("OneElement")) ?= "OneElement\n"

  property("TwoElements") =
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
}
