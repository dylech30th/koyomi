package ink.sora

object ScannerPatterns:
  object BlankCharacter:
    def unapply(c: Char): Option[Char] = Some(c).filter(Set(' ', '\t', '\n', '\r') contains _)
    def ascertain(c: Char): Boolean = unapply(c).isDefined
  end BlankCharacter

  object NonterminalCharacter:
    def unapply(c: Char): Option[Char] = Some(c).filter((('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ List(':', '-')) contains _)
    def ascertain(c: Char): Boolean = unapply(c).isDefined
  end NonterminalCharacter
end ScannerPatterns