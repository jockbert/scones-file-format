package se.mjukomp.scones

import scala.annotation.tailrec

object Scone {
  import scala.language.implicitConversions
  def group(scones: Scone*) = Group(scones.toList)
  implicit def leaf(data: String) = Leaf(data)
  type In = Stream[Char]
  type Scones = List[Scone]
  type Result = Either[ReadError, Scone]
}
import Scone._

sealed trait Scone {}

case class Group(children: List[Scone] = Nil) extends Scone {
  override def toString(): String =
    children.mkString("(", " ", ")")
}
case class Leaf(data: String) extends Scone {
  override def toString(): String = "\"" + data + "\""
}

case class ReadError(
  message: String,
  line:    Int,
  column:  Int,
  before:  String)

/** Parse context */
case class Ctx(in: In, line: Int = 1, column: Int = 0) {
  def dropChar(charsToDrop: Int = 1): Ctx =
    if (charsToDrop <= 0)
      this
    else if (in.head == '\n')
      Ctx(in.tail, line + 1, 1).dropChar(charsToDrop - 1)
    else
      Ctx(in.tail, line, column + 1).dropChar(charsToDrop - 1)
}

case class SconesReader() {

  private def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\n' || c == '\t' || c == '\r'

  @tailrec
  private def trim(ctx: Ctx): Ctx =
    if (ctx.in.isEmpty) ctx
    else if (!isWhitespace(ctx.in.head)) ctx
    else trim(ctx.dropChar())

  @tailrec
  private def parseLeaf(
    ctx:    Ctx,
    result: String = ""): (Ctx, Scone) =
    ctx.in match {
      case Stream.Empty               => (ctx, Leaf(result)) // TODO error handling
      case c #:: _ if isWhitespace(c) => (ctx, Leaf(result))
      case ')' #:: _                  => (ctx, Leaf(result))
      case c #:: _                    => parseLeaf(ctx.dropChar(), result + c)
    }

  @tailrec
  private def parseQuoteLeaf(
    ctx:    Ctx,
    result: String = ""): (Ctx, Scone) =
    ctx.in match {
      case Stream.Empty          => (ctx, Leaf(result)) // TODO error handling
      case '"' #:: tail          => (ctx.dropChar(), Leaf(result))
      case '\\' #:: '"' #:: tail => parseQuoteLeaf(ctx.dropChar(2), result + '"')
      case c #:: tail            => parseQuoteLeaf(ctx.dropChar(), result + c)
    }

  @tailrec
  private def parseList(
    ctx:    Ctx,
    result: Scones = Nil): (Ctx, Scone) =
    ctx.in match {
      case Stream.Empty               => (ctx, Group(result.reverse))
      case ')' #:: _                  => (ctx, Group(result.reverse))
      case c #:: _ if isWhitespace(c) => parseList(trim(ctx), result)
      case '(' #:: _ => {
        val (ctx2, group) = parseParenthesis(ctx)
        parseList(ctx2, group :: result)
      }
      case '"' #:: _ => {
        val (ctx2, leaf) = parseQuoteLeaf(ctx.dropChar())
        parseList(ctx2, leaf :: result)
      }
      case _ => {
        val (ctx2, leaf) = parseLeaf(ctx)
        parseList(ctx2, leaf :: result)
      }
    }

  private def parseParenthesis(
    ctx: Ctx): (Ctx, Scone) =
    ctx.in match {
      case '(' #:: _ => {
        val (ctx2, result) = parseList(ctx.dropChar())
        parseRightParenthesis(ctx2, result)
      }
      case _ => (ctx, Leaf("ERROR EXPECTING LEFT PARENTHESIS")) // TODO error handling
    }

  private def parseRightParenthesis(
    ctx:    Ctx,
    result: Scone): (Ctx, Scone) =
    ctx.in match {
      case ')' #:: _ => (ctx.dropChar(), result)
      case _         => (ctx, Leaf("ERROR EXPECTING RIGHT PARENTHESIS")) // TODO error handling
    }

  private def parse(in: In): Result = {
    val (ctx2, result) = parseList(Ctx(in))

    ctx2.in match {
      case Stream.Empty =>
        Right(parseList(Ctx(in))._2)
      case _ =>
        Left(ReadError(
          "Missing '('",
          ctx2.line,
          ctx2.column,
          ctx2.in.mkString("")))
    }
  }

  def read(in: In): Result = parse(in)

  def read(in: String): Result = read(in.toStream)
}
