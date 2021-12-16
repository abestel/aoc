package io.abestel.aoc.year2020.day5

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

import scala.collection.immutable.BitSet

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map(_.id)
      .fold(BitSet.empty)(_ incl _)
      .compile
      .lastOrError
      .map(
        _.to(LazyList).sliding(2).collectFirst {
          case LazyList(first, second) if first + 1 != second =>
            first + 1
        }
      )
      .flatMap(Console[IO].println(_))

}
