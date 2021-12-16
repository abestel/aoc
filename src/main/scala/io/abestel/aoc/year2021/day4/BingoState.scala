package io.abestel.aoc.year2021.day4

case class BingoState(
    private val drawn: List[Int]
) {
  def +(number: Int): BingoState     = copy(drawn = number +: drawn)
  def contains(number: Int): Boolean = drawn.contains(number)
  def lastDrawn: Option[Int]         = drawn.headOption
  def numberDrawn: Int               = drawn.size
}

object BingoState {
  val empty: BingoState = BingoState(List.empty)
}
