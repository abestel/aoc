package io.abestel.aoc.year2020.day9

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      data          <- dataStream[IO]
      invalidNumber <- invalid[IO](data)
    } yield println(invalidNumber)

}
