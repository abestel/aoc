package io.abestel.aoc.year2020.day11

import cats.effect.{IO, IOApp}
import io.abestel.aoc.utils.grid.{Coordinates, Neighbour}

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  def neighbours(seatMap: SeatMap, coordinates: Coordinates): List[(Coordinates, Element)] =
    seatMap.findFirst(coordinates, Neighbour.all) {
      case Element.Ground => false
      case _              => true
    }.toList

  override def run: IO[Unit] =
    data[IO].map { seatMap =>
      println(seatMap.visualize)

      @tailrec
      def findSynchronization(
          seatMap: SeatMap,
          step: Int = 1,
      ): (Int, SeatMap) = {
        val (newState, changes) = SeatMap.step(5, neighbours).run(seatMap).value
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
