package ink.sora

import scala.annotation.targetName
import scala.reflect.runtime.universe.{Constant, Literal}

extension [T](x: T)
  def let[R](f: T => R): R = f(x)
  def also(f: T => Unit): T = { f(x); x }

  @targetName("pipe")
  def |>[Z](f: T => Z) = f(x)

extension (s: String)
  def escaped(): String = Literal(Constant(s)).toString
  def removeSurrounding(delimiter: String): String =
    s.stripPrefix(delimiter).stripSuffix(delimiter)
    
extension (tokens: List[Token])
  def iterated(): TokenStream = new TokenStream(tokens)