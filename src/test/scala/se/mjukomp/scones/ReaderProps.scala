package se.mjukomp.scones

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.scones.Scone._
import org.scalacheck.Prop
import scala.collection.immutable.Stream

object ReaderProps extends Properties("SconesReader") {

  val reader = SconesReader()

  property("Empty data") =
    readSuccess("", Group())

  property("One leaf") =
    readSuccess(
      "one_element",
      group("one_element"))

  property("Sequence of leafs") =
    readSuccess(
      "some flat data",
      group("some", "flat", "data"))

  property("Nested groups") =
    readSuccess(
      "some (nested (data foo) bar) baz",
      group("some", group("nested", group("data", "foo"), "bar"), "baz"))

  property("Quote leaf") =
    readSuccess(
      "some \"quote data\" foo",
      group("some", "quote data", "foo"))

  property("Escaped quote in leaf") =
    readSuccess(
      "some \"quote \\\"data\\\"\" foo",
      group("some", "quote \"data\"", "foo"))

  property("Padding between leafs") =
    readSuccess(
      "  some  (  padding  )  used  ",
      group("some", group("padding"), "used"))

  property("Whitespace in leaf") =
    readSuccess(
      "\r\n\tsome\r\n\t(\n\n\n\" \r\n\twhitespace\r\n\t \"\r\r\r)\n\t\t\t\tused\n\r\t",
      group("some", group(" \r\n\twhitespace\r\n\t "), "used"))

  property("Missing left parenthesis") =
    readError(
      "missing left parenthesis)",
      ReadError("Missing '('", Position(1, 24)))

  property("Missing right parenthesis") =
    readError(
      "missing (right parenthesis",
      ReadError("Missing ')'", Position(1, 26)))

  property("Missing right quote") =
    readError(
      "missing \"right quote",
      ReadError("Missing closing quote '\"'", Position(1, 20)))

  // --- detected by roundtrip test -------

  property("DoubleBackslashInQuote") =
    readSuccess(
      "\"\\\\\"",
      group("\\"))

  def readSuccess(input: String, expected: Scone): Prop =
    reader.read(input) ?= Right(expected)

  def readError(input: String, expectedError: ReadError): Prop =
    reader.read(input) ?= Left(expectedError)
}
