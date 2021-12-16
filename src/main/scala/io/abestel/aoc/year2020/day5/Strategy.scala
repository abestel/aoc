package io.abestel.aoc.year2020.day5

trait Strategy[T] {
  def split(t: T): Strategy.Split
}

object Strategy {
  sealed trait Split
  object Split {
    case object LowerHalf extends Split
    case object UpperHalf extends Split
  }

  implicit class StrategyOps[T](private val t: T) extends AnyVal {
    def split(implicit strategy: Strategy[T]): Split =
      strategy.split(t)
  }
}
