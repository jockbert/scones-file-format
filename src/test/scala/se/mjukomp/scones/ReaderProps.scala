package se.mjukomp.scones

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.scones.Scone._

object ReaderProps extends Properties("SconesReader") {

  val reader = SconesReader()

  property("Empty data") = reader.
    read("") ?= Group()

  property("One element") = reader.
    read("one_element") ?=
    group("one_element")

  property("Some flat data") = reader.
    read("some flat data") ?=
    group("some", "flat", "data")

  property("Some nested data") = reader.
    read("some (nested (data foo) bar) baz)") ?=
    group("some", group("nested", group("data", "foo"), "bar"), "baz")
}
