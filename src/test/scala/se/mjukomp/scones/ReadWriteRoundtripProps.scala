package se.mjukomp.scones

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Prop
import se.mjukomp.scones.Scone._

object WriteReadRoundtripProps extends Properties("WriteReadRoundtrip") {

  def reader = SconesReader()
  def writer = SconesWriter()

  property("SingleElement") =
    writeReadRoundtrip(Leaf("foo"))

  def writeReadRoundtrip(scones: Scone*): Prop = {
    val expectedRootGroup: Scone = Group(scones.toList)
    val serializedData = writer.write(scones.toList)
    val actualResult: Scone.Result = reader.read(serializedData)

    (actualResult.isRight) :| "Has" &&
      (actualResult.right.get ?= expectedRootGroup) :| "equals expected"
  }

}
