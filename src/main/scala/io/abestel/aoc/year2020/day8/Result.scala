package io.abestel.aoc.year2020.day8

sealed trait Result

object Result {
  case class NormalExit(accumulator: Int)   extends Result
  case class InfiniteLoop(accumulator: Int) extends Result
}
