package io.abestel.aoc.year2020.day3

case class LineMap(
    environment: List[Environment]
) {
  def at(position: Long): Environment =
    environment((position % environment.length).toInt)
}
