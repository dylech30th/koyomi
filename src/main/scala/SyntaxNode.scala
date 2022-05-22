package ink.sora

// enum support is still not good

sealed trait SymbolVariant[T <: SymbolVariant[T]]
case class TerminalSymbol(name: String) extends SymbolVariant[TerminalSymbol]
case class NonterminalSymbol(name: String) extends SymbolVariant[NonterminalSymbol]
case class GroupSymbol(symbols: List[SymbolVariant[_]]) extends SymbolVariant[GroupSymbol]
case class OptionalSymbol(symbols: List[SymbolVariant[_]]) extends SymbolVariant[OptionalSymbol]
case class RepetitionSymbol(symbols: List[SymbolVariant[_]]) extends SymbolVariant[RepetitionSymbol]
object EpsilonSymbol extends TerminalSymbol("ε")

sealed trait SyntaxNode[T <: SyntaxNode[T]]
case class Production(assignee: NonterminalSymbol, alternatives: List[ProductionBody]) extends SyntaxNode[Production]
case class ProductionBody(symbols: List[SymbolVariant[_]]) extends SyntaxNode[ProductionBody]
case class SyntaxDef(productions: List[Production]) extends SyntaxNode[SyntaxDef]