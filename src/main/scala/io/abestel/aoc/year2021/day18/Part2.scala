package io.abestel.aoc.year2021.day18

import cats.effect.{IO, IOApp}
import cats.implicits._

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].compile.toList.map { snailfishes =>
      val maxByMagnitude = (for {
        first  <- snailfishes.iterator
        second <- snailfishes.iterator
        if first != second
      } yield first |+| second).maxBy(_.magnitude)

      println(maxByMagnitude.show)
      println(maxByMagnitude.magnitude)
    }
}
