package io.abestel.aoc.year2020.day6

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map(
        _.iterator
          .map(_.toSet)
          .reduce(_ intersect _)
          .size
      )
      .foldMonoid
      .compile
      .lastOrError
      .flatMap(Console[IO].println(_))

}
