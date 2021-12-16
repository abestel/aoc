package io.abestel.aoc.year2020.day3

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    runSlope[IO](1, 3)
      .flatMap(Console[IO].println)
}
