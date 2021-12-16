package io.abestel.aoc.year2020.day8

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO].map { commands =>
      println(
        runLoop(commands)
      )
    }

}
