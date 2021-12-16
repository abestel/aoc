package io.abestel.aoc.year2020.day5

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {

  override def run: IO[Unit] =
    dataStream[IO]
      .map(_.id)
      .reduce(_ max _)
      .compile
      .lastOrError
      .flatMap(Console[IO].println(_))

}
