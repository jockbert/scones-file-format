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

  property("Flat data") = reader.
    read("some flat data") ?=
    group("some", "flat", "data")

  property("Nested data") = reader.
    read("some (nested (data foo) bar) baz)") ?=
    group("some", group("nested", group("data", "foo"), "bar"), "baz")

  property("Quote data") = reader.
    read("some \"quote data\" foo") ?=
    group("some", "quote data", "foo")

  property("Escaped quote in data") = reader.
    read("some \"quote \\\"data\\\"\" foo") ?=
    group("some", "quote \"data\"", "foo")

  property("Padding") = reader.
    read("  some  (  padding  )  used  ") ?=
    group("some", group("padding"), "used")

  property("Whitespace") = reader.
    read("\r\n\tsome\r\n\t(\n\n\n\" \r\n\twhitespace\r\n\t \"\r\r\r)\n\t\t\t\tused\n\r\t") ?=
    group("some", group(" \r\n\twhitespace\r\n\t "), "used")

}
