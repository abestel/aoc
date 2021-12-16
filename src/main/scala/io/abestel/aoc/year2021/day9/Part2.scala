package io.abestel.aoc.year2021.day9

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map { world =>
        println(world.visualize)
        world.bassins.map(_.cells.size).sorted.reverse.take(3).product
      }
      .map(println(_))

}
