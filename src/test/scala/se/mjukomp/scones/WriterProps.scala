package se.mjukomp.scones

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._

object WriterProps extends Properties("SconesWriter") {

  val writer = SconesWriter()

  property("EmptyData") =
    write() ?= "\n"

  property("OneElement") =
    write(Leaf("OneElement")) ?= "OneElement\n"

  property("TwoElements") =
    write(Leaf("One"), Leaf("Two")) ?=
      """|One
         |Two
         |""".stripMargin

  def write(scones: Scone*) =
    writer.write(scones.toList)
}
