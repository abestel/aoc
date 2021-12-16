package io.abestel.aoc.year2021.day14

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { case (template, recipes) =>
      polymerize(template, recipes)(40)
    }
}
