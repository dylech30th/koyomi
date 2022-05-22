package ink.sora

given Conversion[Scanner, TokenStream] with
  def apply(s: Scanner): TokenStream = s.all().iterated()

final class Parser private (private val tokens: TokenStream):
  private var lookahead: Option[Token] = tokens.next()

  def parse(): SyntaxDef = SyntaxDef(parseSyntaxDef())

  private def parseSyntaxDef(): List[Production] =
    parseProduction() match
      case Some(production) => production :: parseSyntaxDef()
      case None => Nil
  end parseSyntaxDef

  private def parseSymbolVariant(): Option[SymbolVariant[_]] =
    lookahead.flatMap {
      _ match
        case Token(TokenKind.Nonterminal, w) =>
          Some(NonterminalSymbol(w.content)).also(_ => eat(TokenKind.Nonterminal))
        case Token(TokenKind.Terminal, w) =>
          Some(TerminalSymbol(w.content.removeSurrounding("'"))).also(_ => eat(TokenKind.Terminal))
        case Token(TokenKind.LeftParen, _) =>
          eat(TokenKind.LeftParen)
          parseSymbolList() match
            case list @ _ :: _ => Some(GroupSymbol(list)).also(_ => eat(TokenKind.RightParen))
            case Nil => None
        case Token(TokenKind.LeftBracket, _) =>
          eat(TokenKind.LeftBracket)
          parseSymbolList() match
            case list @ _ :: _ => Some(OptionalSymbol(list)).also(_ => eat(TokenKind.RightBracket))
            case Nil => None
        case Token(TokenKind.LeftBrace, _) =>
          eat(TokenKind.LeftBrace)
          parseSymbolList() match
            case list @ _ :: _ => Some(RepetitionSymbol(list)).also(_ => eat(TokenKind.RightBrace))
            case Nil => None
        case _ => None
    }
  end parseSymbolVariant

  private def parseSymbolList(): List[SymbolVariant[_]] =
    parseSymbolVariant() match
      case Some(value) => value :: parseSymbolList()
      case None => Nil
  end parseSymbolList

  private def parseProduction(): Option[Production] =
    lookahead match
      case Some(Token(TokenKind.Nonterminal, w)) =>
        eat(TokenKind.Nonterminal)
        eat(TokenKind.Equal)
        Some(Production(NonterminalSymbol(w.content), ProductionBody(parseSymbolList()) :: parseProductionBodyList()).also(_ => eat(TokenKind.Semicolon)))
      case Some(token) => reportError(s"Expecting nonterminal but $token was found")
      case _ => None
  end parseProduction

  private def parseProductionBodyList(): List[ProductionBody] =
    eat(TokenKind.Alternative)
    parseSymbolList() match
      case list @ _ :: _ => ProductionBody(list) :: parseOptProductionBodyList()
      case Nil => ProductionBody(List(EpsilonSymbol)) :: parseOptProductionBodyList()
  end parseProductionBodyList

  private def parseOptProductionBodyList(): List[ProductionBody] =
    lookahead match
      case Some(Token(TokenKind.Alternative, _)) => parseProductionBodyList()
      case _ => Nil
  end parseOptProductionBodyList

  private def eat(tokenKind: TokenKind): Token =
    if lookahead.exists(_.kind == tokenKind) then
      lookahead.get.also(_ => lookahead = tokens.next())
    else
      throw new ParsingException(s"Expected $tokenKind, but got ${
        lookahead match
          case Some(value) => value.kind
          case None => "EOF"
      } at ${lookahead.map(_.textWindow.rangeLineInfo._1).getOrElse("EOF")}")
  end eat

  private def reportError(message: String): Nothing =
    throw new ParsingException(s"Error at ${lookahead.map(_.textWindow.rangeLineInfo._1).getOrElse("EOF")}: $message")
  end reportError
end Parser

object Parser:
  def apply(tokens: TokenStream): Parser = new Parser(tokens)
end Parser

