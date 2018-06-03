package se.mjukomp.scones

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.scones.Scone._
import org.scalacheck.Prop
import scala.collection.immutable.Stream

object ReaderProps extends Properties("SconesReader") {

  val reader = SconesReader()

  // Parenthesis errors )), ((, () with nice error information
  // Quotes errors - missing end quote
  // print to file
  // roundtrip
  // scheme and scheme validation

  property("Empty data") =
    readSuccess("", Group())

  property("One element") =
    readSuccess(
      "one_element",
      group("one_element"))

  property("Flat data") =
    readSuccess(
      "some flat data",
      group("some", "flat", "data"))

  property("Nested data") =
    readSuccess(
      "some (nested (data foo) bar) baz",
      group("some", group("nested", group("data", "foo"), "bar"), "baz"))

  property("Quote data") =
    readSuccess(
      "some \"quote data\" foo",
      group("some", "quote data", "foo"))

  property("Escaped quote in data") =
    readSuccess(
      "some \"quote \\\"data\\\"\" foo",
      group("some", "quote \"data\"", "foo"))

  property("Padding") =
    readSuccess(
      "  some  (  padding  )  used  ",
      group("some", group("padding"), "used"))

  property("Whitespace") =
    readSuccess(
      "\r\n\tsome\r\n\t(\n\n\n\" \r\n\twhitespace\r\n\t \"\r\r\r)\n\t\t\t\tused\n\r\t",
      group("some", group(" \r\n\twhitespace\r\n\t "), "used"))

  property("Missing left parenthesis") =
    readError(
      "missing left parenthesis)",
      ReadError("Missing '('", 1, 24, ")"))

  def readSuccess(input: String, expected: Scone) =
    reader.read(input) ?= Right(expected)

  def readError(input: String, expectedError: ReadError) =
    reader.read(input) ?= Left(expectedError)
}
