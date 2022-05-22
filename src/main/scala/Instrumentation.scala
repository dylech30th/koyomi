package ink.sora

import scala.annotation.tailrec
import scala.collection.mutable

object Instrumentation:
  def substitute[T <: SyntaxNode[T], S <: SymbolVariant[S]](node: T, variant: S, target: S): T =
    node match
      case Production(assignee, alternatives) =>
        Production(if assignee == variant then target.asInstanceOf[NonterminalSymbol] else assignee, alternatives.map(substitute(_, variant, target)))
      case ProductionBody(alternatives) =>
        ProductionBody(alternatives.map(a => if a == variant then target else a))
      case SyntaxDef(productions) => SyntaxDef(productions.map(p => substitute(p, variant, target)))
  end substitute

  def normalize(productions: List[Production]): List[Production] =
    case class Insertion(afterValue: Int, item: Production)
    case class ValuedProduction(value: Int, production: Production)

    def aux(vp: ValuedProduction): (ValuedProduction, List[Insertion]) =
      val insertions = mutable.Buffer[Insertion]()
      vp.production.alternatives.map { a =>
        a.symbols.map { s =>
          s match
            case GroupSymbol(symbols) =>
              NonterminalSymbol(s"${vp.production.assignee.name}-group")
                .also(r => insertions += Insertion(vp.value, Production(r, List(ProductionBody(symbols)))))
            case OptionalSymbol(symbols) =>
              NonterminalSymbol(s"${vp.production.assignee.name}-opt")
                .also(r => insertions += Insertion(vp.value, Production(r, List(ProductionBody(symbols), ProductionBody(EpsilonSymbol :: Nil)))))
            case RepetitionSymbol(symbols) =>
              val replacement = NonterminalSymbol(s"${vp.production.assignee.name}-rest")
              val optRestSymbol = NonterminalSymbol(s"${vp.production.assignee.name}-rest-opt")
              replacement.also(r => insertions.appendAll(List(
                Insertion(vp.value, Production(r, ProductionBody(symbols :+ optRestSymbol) :: Nil)),
                Insertion(vp.value, Production(optRestSymbol, List(ProductionBody(r :: Nil), ProductionBody(EpsilonSymbol :: Nil))))
              )))
            case _ => s
        } |> ProductionBody.apply
      } |> (p => (ValuedProduction(vp.value, Production(vp.production.assignee, p)), insertions.toList))
    end aux

    @tailrec
    def mutate(buf: mutable.Iterable[ValuedProduction]): List[Production] =
      val insertions = mutable.Buffer[Insertion]()
      val result = buf.map(aux(_) match
        case (p, list @ _ :: _) => p.also(_ => insertions.appendAll(list))
        case (p, Nil) => p
      ).toBuffer
      if insertions.isEmpty then
        result.map(_.production).toList
      else
        for i <- insertions do
          result.insert(result.indexWhere(_.value == i.afterValue) + 1, ValuedProduction(Numbering.nextValue, i.item))
        mutate(result)
    end mutate

    mutate(productions.map(ValuedProduction(Numbering.nextValue, _)).toBuffer)
  end normalize

  def format(list: List[Production]): String =
    def toString(symbols: List[SymbolVariant[_]]): String =
      symbols.map(_ match
        case TerminalSymbol(name) => s"\'$name\'"
        case NonterminalSymbol(name) => name
        case GroupSymbol(symbols) => s"(${toString(symbols)})"
        case OptionalSymbol(symbols) => s"[${toString(symbols)}]"
        case RepetitionSymbol(symbols) => s"{${toString(symbols)}}")
      .mkString(" ")
    list.map { p =>
      s"${p.assignee.name} ::= ${p.alternatives.map(a => toString(a.symbols)).mkString("\n\t| ")}"
    }.mkString("\n")
end Instrumentation