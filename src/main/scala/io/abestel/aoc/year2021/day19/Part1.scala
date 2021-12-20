package io.abestel.aoc.year2021.day19

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { scanners =>
      println(scanners)
    }
}
