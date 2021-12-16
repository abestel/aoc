package io.abestel.aoc.year2020.day2

import cats.effect.{IO, IOApp}
import cats.effect.std.Console

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .filter { case (Rule(minBound, maxBound, pattern), password) =>
        val count = password.count(_ == pattern)
        minBound <= count && count <= maxBound
      }
      .compile
      .count
      .flatMap(Console[IO].println)
}
