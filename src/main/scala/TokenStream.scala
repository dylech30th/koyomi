package ink.sora

final class TokenStream(private val tokens: List[Token]):
  private var index: Int = 0
  
  def next(): Option[Token] = tokens.lift(index.also(_ => index += 1))
end TokenStream
