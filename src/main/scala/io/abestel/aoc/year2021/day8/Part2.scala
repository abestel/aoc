package io.abestel.aoc.year2021.day8

import cats.effect.{IO, IOApp}
import cats.implicits._

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map(
        _.reverse
          .foldLeft(
            (
              /*accumulator*/ 0L,
              /*pow of 10*/ 1L,
            )
          ) { case ((accumulator, powOf10), number) =>
            (
              accumulator + (number.toInt.toLong * powOf10),
              powOf10 * 10,
            )
          }
          ._1
      )
      .foldMonoid
      .compile
      .lastOrError
      .map(println(_))

}
