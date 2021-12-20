package io.abestel.aoc.year2021.day19

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { scanners =>
      println {
        (for {
          probe1 <- scanners.iterator
          probe2 <- scanners.iterator
        } yield probe1.position manhattan probe2.position).max
      }
    }
}