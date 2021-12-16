package io.abestel.aoc.year2020.day5

final case class Seat(
    row: Int,
    column: Int,
) {
  def id: Int =
    row * 8 + column
}
