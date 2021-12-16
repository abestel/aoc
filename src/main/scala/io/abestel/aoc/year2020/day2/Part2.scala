package io.abestel.aoc.year2020.day2

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .filter { case (Rule(minBound, maxBound, pattern), password) =>
        (
          password.lift(minBound - 1).toList ++
            password.lift(maxBound - 1)
        ).count(_ == pattern) == 1
      }
      .compile
      .count
      .flatMap(Console[IO].println)
}
