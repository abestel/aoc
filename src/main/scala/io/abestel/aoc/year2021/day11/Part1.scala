package io.abestel.aoc.year2021.day11

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map { a => println(a.visualize); a }
      .map(Octopuses.steps(100).runA(_).value)
      .map(println(_))
}
