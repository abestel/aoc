package io.abestel.aoc.utils.geom

import cats.Order

case class Pos3(x: Int, y: Int, z: Int) {
  def transform(matrix3: IntMatrix3): Pos3 = {
    val IntMatrix3(_00, _01, _02, _10, _11, _12, _20, _21, _22) = matrix3

    Pos3(
      x = _00 * x + _01 * y + _02 * z,
      y = _10 * x + _11 * y + _12 * z,
      z = _20 * x + _21 * y + _22 * z,
    )
  }

  def +(pos3: Pos3): Pos3 =
    Pos3(
      x = x + pos3.x,
      y = y + pos3.y,
      z = z + pos3.z,
    )

  def -(pos3: Pos3): Pos3 =
    Pos3(
      x = x - pos3.x,
      y = y - pos3.y,
      z = z - pos3.z,
    )

  def manhattan(pos3: Pos3): Int =
    (x - pos3.x).abs + (y - pos3.y).abs + (z - pos3.z).abs
}

object Pos3 {
  implicit val order: Order[Pos3] = Order.fromOrdering(
    Ordering
      .by[Pos3, Int](_.x)
      .orElse(Ordering.by(_.y))
      .orElse(Ordering.by(_.z))
  )
}
