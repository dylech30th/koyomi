package ink.sora

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

final class Scanner(private val sourceText: SourceText):
  def all(): List[Token] =
    sourceText.reset()
    def loop(): List[Token] =
      next() match
        case Some(token) =>
          token :: loop()
        case None => Nil
    loop()
  end all

  def next(): Option[Token] =
    for
      ch <- sourceText.current()
      token <- ch match
        case ScannerPatterns.BlankCharacter(_) =>
          sourceText.advanceWhile(ScannerPatterns.BlankCharacter.ascertain)
          sourceText.catchUp()
          next()
        case '\'' => scanTerminal()
        case '[' => sourceText.one().map(Token(TokenKind.LeftBracket, _))
        case ']' => sourceText.one().map(Token(TokenKind.RightBracket, _))
        case '(' =>
          if sourceText.peek() contains '*' then
            sourceText.advance(2)
            trySkipComment()
            next()
          else sourceText.one().map(Token(TokenKind.LeftParen, _))
        case ')' => sourceText.one().map(Token(TokenKind.RightParen, _))
        case '{' => sourceText.one().map(Token(TokenKind.LeftBrace, _))
        case '}' => sourceText.one().map(Token(TokenKind.RightBrace, _))
        case '|' => sourceText.one().map(Token(TokenKind.Alternative, _))
        case ';' => sourceText.one().map(Token(TokenKind.Semicolon, _))
        case '=' => sourceText.one().map(Token(TokenKind.Equal, _))
        case ScannerPatterns.NonterminalCharacter(_) => scanNonterminal()
        case c => reportError(s"Unexpected character: $c", sourceText.position())
    yield token
  end next

  @tailrec
  private def trySkipComment(): Unit =
    sourceText.advanceWhile(_ != '*')
    sourceText.peek() match
      case Some(')') =>
        sourceText.advance(2)
        sourceText.catchUp()
      case _ => trySkipComment()
  end trySkipComment

  private def scanTerminal(): Option[Token] =
    sourceText.advance()
    sourceText.advanceWhile(_ != '\'')
    sourceText.advance() // consume '
    val window = sourceText.windowAndCatchUp()
    if window.exists(w => w.content.length <= 2 || !w.content.filter(_ != '\'').forall(ScannerPatterns.NonterminalCharacter.ascertain)) then
      reportError("A terminal sequence must contains at least one character and must consists of only letters and digits.", window.get.rangeLineInfo._1)
    window.map(w => Token(TokenKind.Terminal, w))
  end scanTerminal

  private def scanNonterminal(): Option[Token] =
    sourceText.advanceWhile(ScannerPatterns.NonterminalCharacter.ascertain)
    sourceText.windowAndCatchUp().map(w => Token(TokenKind.Nonterminal, w))

  private def reportError(message: String, position: LineInfo): Nothing =
    throw new ScannerException(s"Error at $position: $message")
end Scanner