package io.abestel.aoc.year2021.day11

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map { octopuses =>
        println(octopuses.visualize)

        @tailrec
        def findSynchronization(
            octopuses: Octopuses,
            step: Int = 1,
        ): Int = {
          val (newState, numberFlashes) = Octopuses.step.run(octopuses).value
          if (numberFlashes == octopuses.values.size) {
            step
          } else {
            findSynchronization(newState, step + 1)
          }
        }

        findSynchronization(octopuses)
      }
      .map(println)
}
