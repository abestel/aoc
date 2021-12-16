package io.abestel.aoc.year2021.day1

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    transformAndCount[IO](
      _.sliding(3)
        .map(
          _.iterator.sum
        )
    )
}
