package io.abestel.aoc.year2020.day7

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  private val target: BagName = "shiny gold"

  override def run: IO[Unit] =
    dataStream[IO].compile.toList
      .map(_.toMap)
      .map(BagRules)
      .map { ruleMap =>
        ruleMap.rules.keys
          .to(LazyList)
          .count(ruleMap.contains(_, target))
      }
      .map(println(_))
}
