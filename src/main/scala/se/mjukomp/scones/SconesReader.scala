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

case class Position(line: Int = 1, column: Int = 0) {
  def moveRight() = Position(line, column + 1)
  def moveNewLine() = Position(line + 1, column)
  def move(newLine: Boolean) =
    if (newLine) moveNewLine() else moveRight()
}

case class ReadError(
  message:  String,
  position: Position)

/** Parse context */
case class Ctx(in: In, pos: Position = Position()) {
  def dropChar(charsToDrop: Int = 1): Ctx =
    if (charsToDrop <= 0) this
    else Ctx(in.tail, pos.move(in.head == '\n')).dropChar(charsToDrop - 1)

  def error(message: String) =
    Left(ReadError(message, pos))
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
      case Stream.Empty               => (ctx, Leaf(result))
      case c #:: _ if isWhitespace(c) => (ctx, Leaf(result))
      case ')' #:: _                  => (ctx, Leaf(result))
      case c #:: _                    => parseLeaf(ctx.dropChar(), result + c)
    }

  @tailrec
  private def parseQuoteLeaf(
    ctx:    Ctx,
    result: String = ""): (Ctx, Result) =
    ctx.in match {
      case Stream.Empty          => (ctx, ctx.error("Missing closing quote '\"'"))
      case '"' #:: tail          => (ctx.dropChar(), Right(Leaf(result)))
      case '\\' #:: '"' #:: tail => parseQuoteLeaf(ctx.dropChar(2), result + '"')
      case c #:: tail            => parseQuoteLeaf(ctx.dropChar(), result + c)
    }

  @tailrec
  private def parseList(
    ctx:    Ctx,
    result: Scones = Nil): (Ctx, Result) =
    ctx.in match {
      case Stream.Empty               => (ctx, Right(Group(result.reverse)))
      case ')' #:: _                  => (ctx, Right(Group(result.reverse)))
      case c #:: _ if isWhitespace(c) => parseList(trim(ctx), result)
      case '(' #:: _ => {
        val (ctx2, group) = parseParenthesis(ctx)
        if (group.isLeft)
          (ctx2, group)
        else
          parseList(ctx2, (group.right.get) :: result)
      }
      case '"' #:: _ => {
        val (ctx2, res2) = parseQuoteLeaf(ctx.dropChar())
        if (res2.isLeft)
          (ctx2, res2)
        else
          parseList(ctx2, res2.right.get :: result)
      }
      case _ => {
        val (ctx2, leaf) = parseLeaf(ctx)
        parseList(ctx2, leaf :: result)
      }
    }

  private def parseParenthesis(
    ctx: Ctx): (Ctx, Result) =
    ctx.in match {
      case '(' #:: _ => {
        val (ctx2, result) = parseList(ctx.dropChar())
        if (result.isLeft)
          (ctx2, result)
        else
          parseRightParenthesis(ctx2, result.right.get)
      }
      case _ => (ctx, ctx.error("Missing '('"))
    }

  private def parseRightParenthesis(
    ctx:    Ctx,
    result: Scone): (Ctx, Result) =
    ctx.in match {
      case ')' #:: _ => (ctx.dropChar(), Right(result))
      case _         => (ctx, ctx.error("Missing ')'"))
    }

  private def parse(in: In): Result = {
    val (ctx2, result) = parseList(Ctx(in))

    ctx2.in match {
      case Stream.Empty => result
      case _            => ctx2.error("Missing '('")
    }
  }

  def read(in: In): Result = parse(in)

  def read(in: String): Result = read(in.toStream)
}
