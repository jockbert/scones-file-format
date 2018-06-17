package se.mjukomp.scones

import java.io.PrintWriter

import org.scalacheck.Prop
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import se.mjukomp.scones.Generators.sconesGen
import se.mjukomp.scones.Scone.Scones

object WriteReadRoundtripProps extends Properties("WriteReadRoundtrip") {

  def reader = SconesReader()
  def writer = SconesWriter()

  property("SingleElement") =
    writeReadRoundtrip(Leaf("foo"))

  property("AnyElements") = forAll(sconesGen) { scones: Scones =>
    writeReadRoundtrip(scones: _*)
  }

  def writeReadRoundtrip(scones: Scone*): Prop = {
    val expectedRootGroup: Scone = Group(scones.toList)
    val serializedData = writer.write(scones.toList)
    val actualResult: Scone.Result = reader.read(serializedData)

    if (actualResult.isLeft) {
      val writer = new PrintWriter("Foop.txt")
      writer.println(serializedData)
      writer.close()
      throw new RuntimeException("Unexpected left in " + actualResult)
    }

    (actualResult.isRight :| "Is right projection") &&
      ((actualResult.right.get ?= expectedRootGroup) :| "actual equals expected")
  }
}
