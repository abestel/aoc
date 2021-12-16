package io.abestel.aoc.year2021.day8

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map {
        _.count {
          case Number.One | Number.Four | Number.Seven | Number.Eight => true
          case _                                                      => false
        }
      }
      .foldMonoid
      .compile
      .lastOrError
      .map(println(_))
}
