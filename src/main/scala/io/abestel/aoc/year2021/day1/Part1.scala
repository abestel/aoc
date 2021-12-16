package io.abestel.aoc.year2021.day1

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    transformAndCount[IO](identity)
}
