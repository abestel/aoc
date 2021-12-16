package io.abestel.aoc.year2020.day11

import cats.effect.{IO, IOApp}
import io.abestel.aoc.utils.grid.Neighbour

import scala.annotation.tailrec

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { seatMap =>
      println(seatMap.visualize)

      @tailrec
      def findSynchronization(
          seatMap: SeatMap,
          step: Int = 1,
      ): (Int, SeatMap) = {
        val (newState, changes) = SeatMap
          .step(4, _.adjacent(_, Neighbour.all).toList)
          .run(seatMap)
          .value

        if (changes == 0) {
          (step, newState)
        } else {
          findSynchronization(newState, step + 1)
        }
      }

      val (steps, newState) = findSynchronization(seatMap)
      println(steps)
      println(newState.values.values.count(_.isOccupied))
    }
}
