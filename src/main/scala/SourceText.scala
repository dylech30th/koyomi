package ink.sora

import scala.annotation.tailrec

case class TextWindow(content: String, range: Range, rangeLineInfo: (LineInfo, LineInfo))

object TextWindow:
  val empty: TextWindow = TextWindow("", 0 to 0, (LineInfo(0, 0), LineInfo(0, 0)))
end TextWindow

given Conversion[String, SourceText] with
  def apply(s: String): SourceText = SourceText(s)

final class SourceText(text: String):
  private var index: Int = 0
  private var forward: Int = 0

  def reset(): Unit = { index = 0; forward = 0}

  def current(): Option[Char] = if index < text.length then Some(text(index)) else None

  def pioneer(): Option[Char] = if forward < text.length then Some(text(forward)) else None

  def peek(n: Int = 1): Option[Char] = if forward + n < text.length then Some(text(forward + n)) else None

  def next(): Option[Char] = { advance(); pioneer() }

  def advance(n: Int = 1): Unit = forward += n

  def catchUp(): Unit = index = forward

  def window(): Option[TextWindow] =
    if forward <= text.length then
      Some(
        TextWindow(text.slice(index, forward), index until forward, (positionOf(index), positionOf(forward)))
      )
    else None

  def windowAndCatchUp(): Option[TextWindow] = window().also(_ => catchUp())

  def ret(): Unit = forward -= 1

  def one(): Option[TextWindow] = { advance(); windowAndCatchUp() }

  def getAndAdvance(n: Int = 1): Option[Char] = pioneer().also(_ => advance(n))

  def advanceAndGet(n: Int = 1): Option[Char] = { advance(n); pioneer() }

  def position(): LineInfo = positionOf(forward)

  private def positionOf(index: Int): LineInfo =
    val slice = text.slice(0, index.min(text.length))
    LineInfo(slice.count(_ == '\n') + 1, index - slice.lastIndexOf('\n'))

  @tailrec
  def advanceWhile(predicate: Char => Boolean): Unit =
    pioneer() match
      case Some(c) if predicate(c) =>
        advance()
        advanceWhile(predicate)
      case _ => ()
end SourceText

object SourceText:
  val EOF: Char = '\u0000'
end SourceText