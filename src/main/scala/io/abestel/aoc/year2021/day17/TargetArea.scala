package io.abestel.aoc.year2021.day17

case class TargetArea(minX: Int, maxX: Int, minY: Int, maxY: Int) {
  def contains(x: Int, y: Int): Boolean =
    minX <= x && x <= maxX && minY <= y && y <= maxY
}