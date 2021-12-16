package io.abestel.aoc.year2021.day9

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map { world =>
        world.lowPoints.map { case (_, height) => height + 1 }.sum
      }
      .map(println(_))
}
