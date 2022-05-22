package ink.sora

case class LineInfo(lineNumber: Int, columnNumber: Int):
  override def toString: String = s"$lineNumber:$columnNumber"
end LineInfo