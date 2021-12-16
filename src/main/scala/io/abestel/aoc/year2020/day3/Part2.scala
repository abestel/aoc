package io.abestel.aoc.year2020.day3

import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import cats.implicits._

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    List(
      1 -> 1,
      1 -> 3,
      1 -> 5,
      1 -> 7,
      2 -> 1,
    ).traverse { case (downStep, rightStep) =>
      runSlope[IO](downStep, rightStep)
    }.map(_.product)
      .flatMap(Console[IO].println)

}
