package se.mjukomp.scones

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._

object WriterProps extends Properties("SconesWriter") {

  val writer = SconesWriter()

  property("EmptyData") =
    writer.write(Nil) ?= Stream.empty[Char]

  property("OneElement") =
    writer.write(Leaf("OneElement") :: Nil) ?= "OneElement".toStream

}
