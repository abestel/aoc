package io.abestel.aoc.year2021.day7

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map { positions =>
        val posMin = positions.min
        val posMax = positions.max

        val minCostAndGoal =
          (posMin to posMax)
            .to(LazyList)
            .map { goal =>
              goal -> positions.map(_ - goal).map(_.abs).sum
            }
            .reduce[(Int, Int)] { case ((goal1, cost1), (goal2, cost2)) =>
              if (cost1 <= cost2) {
                goal1 -> cost1
              } else {
                goal2 -> cost2
              }
            }

        println(
          minCostAndGoal
        )
      }
}
