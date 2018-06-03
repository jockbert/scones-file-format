package se.mjukomp.scones

import scala.annotation.tailrec

object Scone {
  def group(scones: Scone*) = Group(scones.toList)
  implicit def leaf(data: String) = Leaf(data)
}

sealed trait Scone {}

case class Group(children: List[Scone] = Nil) extends Scone {
  override def toString(): String =
    children.mkString("(", " ", ")")
}
case class Leaf(data: String) extends Scone {
  override def toString(): String = data
}

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
      case ')' #:: tail => (in, result)
      case c #:: tail   => parseData(tail, result + c)
    }

  private def breakTailrecParseList(in: In): (In, Scones) =
    parseList(in)

  @tailrec
  private def parseList(
    in:     In,
    result: Scones = Nil): (In, Scones) =
    in match {
      case Stream.Empty => (in, result)
      case ' ' #:: tail => parseList(tail, result)
      case ')' #:: tail => (tail, result)
      case '(' #:: tail => {
        val (in2, group) = breakTailrecParseList(tail)
        parseList(in2, Group(group.reverse) :: result)
      }
      case _ => {
        val (in2, data) = parseData(in)
        parseList(in2, Leaf(data) :: result)
      }
    }

  private def parse(in: In): Scone =
    Group(parseList(in)._2.reverse)

  def read(in: In): Scone = parse(in)

  def read(in: String): Scone = read(in.toStream)
}
