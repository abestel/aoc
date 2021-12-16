package io.abestel.aoc.year2021.day4

import cats.ApplicativeError
import cats.implicits._

case class DrawOrder(
    numbers: List[Int]
)

object DrawOrder {
  def parse[F[_]](rawList: String)(implicit F: ApplicativeError[F, Throwable]): F[DrawOrder] =
    rawList
      .split(",")
      .toList
      .traverse(rawNumber =>
        F.fromOption(
          rawNumber.toIntOption,
          new IllegalArgumentException(s"'$rawNumber' is not a valid number"),
        )
      )
      .map(DrawOrder(_))
}
