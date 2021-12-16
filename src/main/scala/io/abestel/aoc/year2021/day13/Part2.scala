package io.abestel.aoc.year2021.day13

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]("day13_2").map(t => println(t.marked.size))
}
