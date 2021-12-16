package io.abestel.aoc.year2021.day13

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]("day13_1").map(t => println(t.marked.size))
}
