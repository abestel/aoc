package io.abestel.aoc.year2020.day6

import cats.effect.{IO, IOApp}
import cats.effect.std.Console

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map(
        _.iterator
          .map(_.toSet)
          .reduce(_ union _)
          .size
      )
      .foldMonoid
      .compile
      .lastOrError
      .flatMap(Console[IO].println(_))

}
