package io.abestel.aoc.year2020.day7

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  private val target: BagName = "shiny gold"

  override def run: IO[Unit] =
    dataStream[IO].compile.toList
      .map(_.toMap)
      .map(BagRules)
      .map(_.capacity(target))
      .map(println(_))
}
