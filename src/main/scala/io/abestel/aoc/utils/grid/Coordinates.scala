package io.abestel.aoc.utils.grid

case class Coordinates(x: X, y: Y) {
  def transform(deltaX: Int, deltaY: Int): Coordinates =
    copy(
      x = X(x.value + deltaX),
      y = Y(y.value + deltaY),
    )
}

object Coordinates {
  implicit val order: Ordering[Coordinates] =
    Ordering
      .by[Coordinates, Int](_.y.value)
      .orElse(
        Ordering.by(_.x.value)
      )
}
