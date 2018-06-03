package se.mjukomp.scones

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.scones.Scone._

object ReaderProps extends Properties("SconesReader") {

  val reader = SconesReader()

  property("Empty data") = reader.
    read("") ?= Parent()

  property("One element") = reader.
    read("one_element") ?= p(l("one_element"))

  property("Some flat data") = reader.
    read("some flat data") ?= p(l("some"), l("flat"), l("data"))
}
