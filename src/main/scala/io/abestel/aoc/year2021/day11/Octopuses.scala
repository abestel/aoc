package io.abestel.aoc.year2021.day11

import cats.data.State
import cats.implicits._
import io.abestel.aoc.utils.grid._

import scala.annotation.tailrec

case class Octopuses(
    override val values: Map[Coordinates, Int]
) extends Grid2D[Int] {
  override def formatCell(coordinates: Coordinates, energy: Int): String =
    if (0 == energy) {
      s"${Console.BOLD}${Console.GREEN_B}$energy${Console.RESET}"
    } else {
      energy.toString
    }

  def mapValues(fn: Int => Int): Octopuses =
    copy(
      values.view.mapValues(fn).toMap
    )

  def update(updated: Map[Coordinates, Int]): Octopuses =
    copy(
      values ++ updated
    )
}

object Octopuses extends Grid2DCompanion[Int] {
  def apply(energies: List[List[Int]]): Octopuses =
    Octopuses(makeIndexedMap(energies))

  def step: State[Octopuses, Int] = State[Octopuses, Int] { octopuses =>
    @tailrec
    def go(
        state: Octopuses,
        flashes: Int = 0,
    ): (Octopuses, Int) = {
      state.values.iterator.collectFirst { case (coordinates, energy) if energy > 9 => coordinates } match {
        case None =>
          (state, flashes)

        case Some(coordinates) =>
          val updatedNeighbours =
            state
              .adjacent(coordinates, Neighbour.all)
              .filterNot { case (_, energy) => energy == 0 } // Has already flashed at this step
              .toMap
              .view
              .mapValues(_ + 1)
              .toMap

          go(
            state = state.update(updatedNeighbours ++ Map(coordinates -> 0)),
            flashes = flashes + 1,
          )
      }
    }

    go(
      octopuses.mapValues(_ + 1)
    )
  }.flatTap(_ => visualize())

  def steps(n: Int): State[Octopuses, Int] =
    (0 until n).toList.traverse(_ => step).map(_.sum)
}
