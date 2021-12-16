package io.abestel.aoc.year2021.day14

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { case (template, recipes) =>
      polymerize(template, recipes)(10)
    }
}
