package io.abestel.aoc.year2021.day15

import cats.effect.{IO, IOApp}
import io.abestel.aoc.utils.grid._

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] = data[IO].map { ceiling =>
    // Tile the ceiling
    val tiled = for {
      i <- 0 until 5
      j <- 0 until 5
      x <- ceiling.xRange
      y <- ceiling.yRange
    } yield {
      val newCoords = Coordinates(
        X(x.value + ((ceiling.xMax.value + 1) * i)),
        Y(y.value + ((ceiling.yMax.value + 1) * j)),
      )

      val value = (ceiling.values(Coordinates(x, y)) + i + j - 1) % 9 + 1

      newCoords -> value
    }

    val tiledCeiling = Ceiling(tiled.toMap)
    println(tiledCeiling.visualize)
    println(tiledCeiling.minimalRisk)
  }
}
