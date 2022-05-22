package ink.sora

object Numbering:
  private var value: Int = 1

  def nextValue: Int =
    value += 1
    value
  end nextValue
end Numbering