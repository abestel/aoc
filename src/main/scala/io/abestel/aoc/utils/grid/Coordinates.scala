package io.abestel.aoc.utils.grid

case class Coordinates(x: X, y: Y) {
  def transform(deltaX: Int, deltaY: Int): Coordinates =
    copy(
      x = X(x.value + deltaX),
      y = Y(y.value + deltaY),
    )
}
