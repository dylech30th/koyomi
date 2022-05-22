package ink.sora


case class Token(kind: TokenKind, textWindow: TextWindow):
  override def toString: String = s"Token($kind, TextWindow(${textWindow.content.escaped()}, ${textWindow.range.start}..${textWindow.range.end}, ${textWindow.rangeLineInfo}))"
end Token

enum TokenKind:
  case Apostrophe extends TokenKind
  case Semicolon extends TokenKind
  case LeftBracket extends TokenKind
  case RightBracket extends TokenKind
  case LeftBrace extends TokenKind
  case RightBrace extends TokenKind
  case LeftParen extends TokenKind
  case RightParen extends TokenKind
  case Equal extends TokenKind
  case Asterisk extends TokenKind
  case Alternative extends TokenKind
  case Terminal extends TokenKind
  case Nonterminal extends TokenKind
end TokenKind