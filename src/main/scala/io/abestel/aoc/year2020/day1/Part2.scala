package io.abestel.aoc.year2020.day1

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO].compile.toList
      .map { numbers =>
        (for {
          n1 <- numbers.to(LazyList)
          n2 <- numbers.to(LazyList)
          n3 <- numbers.to(LazyList)
          if n1 + n2 + n3 == 2020
        } yield n1 * n2 * n3).headOption
      }
      .flatMap(Console[IO].println)
}
