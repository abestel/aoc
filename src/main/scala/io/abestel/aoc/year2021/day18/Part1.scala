package io.abestel.aoc.year2021.day18

import cats.effect.{IO, IOApp}
import cats.syntax.show._

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].reduceSemigroup
      .debug(_.show)
      .compile
      .lastOrError
      .map { snailfish =>
        println(snailfish.magnitude)
      }
}
