package se.mjukomp.scones

import scala.annotation.tailrec

object Scone {
  import scala.language.implicitConversions
  def group(scones: Scone*) = Group(scones.toList)
  implicit def leaf(data: String) = Leaf(data)

  type In = Stream[Char]
  type Scones = List[Scone]
  type Result = Either[ReadError, Scone]
  type EmptyCtx = Ctx[Unit]
  type ResultCtx = Ctx[Result]
  type StringCtx = Ctx[String]
  type SconeCtx = Ctx[Scone]
  type SconeListCtx = Ctx[List[Scone]]

  def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\n' || c == '\t' || c == '\r'
}
import Scone._

sealed trait Scone {}

case class Group(children: List[Scone] = Nil) extends Scone {
  override def toString(): String =
    children.mkString("group(", " ", ")")
}
case class Leaf(data: String) extends Scone {
  override def toString(): String = "leaf(" + data + ")"
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

/** ParseContext */
case class Ctx[R](in: In, pos: Position = Position(), result: R) {

  def dropChar(charsToDrop: Int = 1): Ctx[R] =
    if (charsToDrop <= 0) this
    else Ctx(in.tail, pos.move(in.head == '\n'), result)
      .dropChar(charsToDrop - 1)

  def withResult[S](s: S) = Ctx[S](in, pos, s)

  def error(message: String) =
    Left(ReadError(message, pos))

  def trim(): Ctx[R] =
    if (in.isEmpty) this
    else if (!isWhitespace(in.head)) this
    else dropChar().trim()

  def map[Q](fn: R => Q): Ctx[Q] =
    Ctx[Q](in, pos, fn(result))

  def noResult: EmptyCtx = withResult(Unit)
}

case class SconesReader() {

  private def parseLeaf(ctx: EmptyCtx): SconeCtx =
    parseLeaf2(ctx.withResult(""))

  @tailrec
  private def parseLeaf2(ctx: StringCtx): SconeCtx =
    ctx.in match {
      case Stream.Empty               => ctx.map(Leaf(_))
      case c #:: _ if isWhitespace(c) => ctx.map(Leaf(_))
      case ')' #:: _                  => ctx.map(Leaf(_))
      case c #:: _                    => parseLeaf2(ctx.dropChar().map(_ + c))
    }

  private def parseQuoteLeaf(ctx: EmptyCtx): ResultCtx =
    parseQuoteLeaf2(ctx.withResult(""))

  @tailrec
  private def parseQuoteLeaf2(ctx: StringCtx): ResultCtx =
    ctx.in match {
      case Stream.Empty          => ctx.withResult(ctx.error("Missing closing quote '\"'"))
      case '"' #:: tail          => ctx.dropChar().map(result => Right(Leaf(result)))
      case '\\' #:: '"' #:: tail => parseQuoteLeaf2(ctx.dropChar(2).map(_ + '"'))
      case c #:: tail            => parseQuoteLeaf2(ctx.dropChar().map(_ + c))
    }

  private def parseList(ctx: EmptyCtx): ResultCtx =
    parseList2(ctx.withResult[List[Scone]](Nil))

  @tailrec
  private def parseList2(ctx: SconeListCtx): ResultCtx =
    ctx.in match {
      case Stream.Empty               => ctx.map(result => Right(Group(result.reverse)))
      case ')' #:: _                  => ctx.map(result => Right(Group(result.reverse)))
      case c #:: _ if isWhitespace(c) => parseList2(ctx.trim())
      case '(' #:: _ => {
        val ctx2 = parseParenthesis(ctx.noResult)
        if (ctx2.result.isLeft)
          ctx2
        else
          parseList2(ctx2.map(res2 => res2.right.get :: ctx.result))
      }
      case '"' #:: _ => {
        val ctx2 = parseQuoteLeaf(ctx.dropChar().noResult)
        if (ctx2.result.isLeft) ctx2
        else parseList2(ctx2.map(res2 => res2.right.get :: ctx.result))
      }
      case _ => {
        val ctx2 = parseLeaf(ctx.noResult)
        parseList2(ctx2.map(_ :: ctx.result))
      }
    }

  private def parseParenthesis(
    ctx: EmptyCtx): ResultCtx =
    ctx.in match {
      case '(' #:: _ => {
        val ctx2: ResultCtx = parseList(ctx.dropChar().noResult)
        if (ctx2.result.isLeft) ctx2
        else parseRightParenthesis(ctx2)
      }
      case _ => ctx.withResult(ctx.error("Missing '('"))
    }

  private def parseRightParenthesis(
    ctx: ResultCtx): ResultCtx =
    ctx.in match {
      case ')' #:: _ => ctx.dropChar()
      case _         => ctx.withResult(ctx.error("Missing ')'"))
    }

  private def parse(in: In): Result = {
    val startContext: EmptyCtx = Ctx(in, Position(), ())
    val ctx2 = parseList(startContext)

    ctx2.in match {
      case Stream.Empty => ctx2.result
      case _            => ctx2.error("Missing '('")
    }
  }

  def read(in: In): Result = parse(in)

  def read(in: String): Result = read(in.toStream)
}
