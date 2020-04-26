package com.topaz.quantity

case class Taint(value: Int) extends AnyVal {
  def |(other: Taint): Taint = Taint(value | other.value)

  def hasTaint(taint: Taint): Boolean = (value & taint.value) == taint.value

  override def toString = this match {
    case Taint.clean => "Clean"
    case Taint.constant => "Constant"
    case Taint.marketData => "Market data"
    case Taint.tradeData => "Trade data"
    case Taint.interestingExplanation => "Explanation to this point is interesting"
    case Taint.postExerciseOptionGuess => "Post expiry option value may be incorrect"
    case Taint.failedToValuePhysicalOptionality => "Failed to value physical optionality"
    case _ => s"No string value for $value"
  }
}

object Taint {
  val clean: Taint = Taint(0)
  val constant: Taint = Taint(1 << 1)
  val marketData: Taint = Taint(1 << 2)
  val tradeData: Taint = Taint(1 << 3)
  val interestingExplanation: Taint = Taint(1 << 4)
  val postExerciseOptionGuess: Taint = Taint(1 << 5)
  val failedToValuePhysicalOptionality: Taint = Taint(1 << 6)

  def combine(taint: TraversableOnce[Taint]): Taint = taint.foldLeft(clean)(_ | _)

  def taint(qtys: Qty*): Taint = combine(qtys.map(_.taint))
}
