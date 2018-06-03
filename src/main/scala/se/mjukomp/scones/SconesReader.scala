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
  private def parseLeaf(
    in:     In,
    result: String = ""): (In, Leaf) =
    in match {
      case Stream.Empty => (in, Leaf(result))
      case ' ' #:: tail => (in, Leaf(result))
      case ')' #:: tail => (in, Leaf(result))
      case c #:: tail   => parseLeaf(tail, result + c)
    }

  private def breakTailrecParseList(in: In): (In, Group) =
    parseList(in)

  @tailrec
  private def parseList(
    in:     In,
    result: Scones = Nil): (In, Group) =
    in match {
      case Stream.Empty => (in, Group(result.reverse))
      case ')' #:: tail => (tail, Group(result.reverse))
      case ' ' #:: tail => parseList(tail, result)
      case '(' #:: tail => {
        val (in2, group) = breakTailrecParseList(tail)
        parseList(in2, group :: result)
      }
      case _ => {
        val (in2, leaf) = parseLeaf(in)
        parseList(in2, leaf :: result)
      }
    }

  private def parse(in: In): Scone = parseList(in)._2

  def read(in: In): Scone = parse(in)

  def read(in: String): Scone = read(in.toStream)
}
