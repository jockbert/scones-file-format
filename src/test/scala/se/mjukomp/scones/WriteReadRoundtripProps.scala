package se.mjukomp.scones

import java.io.PrintWriter

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import se.mjukomp.scones.Generators.sconeGen
import se.mjukomp.scones.Generators.shrinkScone

object WriteReadRoundtripProps extends Properties("WriteReadRoundtrip") {

  def reader = SconesReader()
  def writer = SconesWriter()

  property("SingleElement") =
    writeReadRoundtrip(Leaf("foo"))

  property("AnyElements") = forAll(sconeGen) {
    writeReadRoundtrip _
  }

  def writeReadRoundtrip(scone: Scone): Prop = {
    val expectedRootGroup: Scone = Group(scone :: Nil)
    val serializedData = writer.write(scone)

    val actualResult: Scone.Result = reader.read(serializedData)

    if (actualResult.isLeft) {
      val writer = new PrintWriter("data.txt")
      writer.println(serializedData)
      writer.close()

      return false :|
        ("    Expected successful read but got:\n" +
          "    " + actualResult.left.get + "\n" +
          "    Serialized data written to file data.txt")
    }

    (actualResult.isRight :| "Is right projection") &&
      ((actualResult.right.get == expectedRootGroup) :| "Read " +
        (actualResult.right.get match {
          case Group(content) => content.mkString("'", " ", "'")
          case _              => throw new RuntimeException("Unexpected Leaf")
        }))
  }
}
