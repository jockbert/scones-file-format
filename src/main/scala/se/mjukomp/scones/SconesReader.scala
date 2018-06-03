package se.mjukomp.scones

import scala.annotation.tailrec

object Scone {
  def p(scones: Scone*) = Parent(scones.toList)
  def l(data: String) = Leaf(data)
}

sealed trait Scone {}

case class Parent(children: List[Scone] = Nil) extends Scone
case class Leaf(data: String) extends Scone

case class SconesReader() {
  import Scone._

  type In = Stream[Char]
  type Scones = List[Scone]

  @tailrec
  private def parseData(
    in:     In,
    result: String = ""): (In, String) =
    in match {
      case Stream.Empty => (in, result)
      case ' ' #:: tail => (in, result)
      case c #:: tail   => parseData(tail, result + c)
    }

  @tailrec
  private def parseList(
    in:     In,
    result: Scones = Nil): (In, Scones) =
    in match {
      case Stream.Empty => (in, result)
      case ' ' #:: tail => parseList(tail, result)
      case _ => {
        val (in2, data) = parseData(in)
        parseList(in2, Leaf(data) :: result)
      }
    }

  private def parse(in: In): Scone =
    Parent(parseList(in)._2.reverse)

  def read(in: In): Scone = parse(in)

  def read(in: String): Scone = read(in.toStream)
}
