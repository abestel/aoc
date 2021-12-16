package io.abestel.aoc.year2021.day6

import cats.effect.{IO, IOApp}
import fs2.Stream

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .flatMap { fishWorld =>
        Stream
          .iterate(1)(_ + 1)
          .take(256)
          .fold(fishWorld) { case (fishWorld, _) => fishWorld.nextDay }
      }
      .compile
      .lastOrError
      .map(fishWorld => println(fishWorld.ageToPopulation.values.sum))
}
