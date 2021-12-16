package io.abestel.aoc.year2021.day15

import cats.implicits._
import io.abestel.aoc.utils.grid._

import scala.annotation.tailrec

case class Ceiling(override val values: Map[Coordinates, Int]) extends Grid2D[Int] {
  type IdxVal = (Coordinates, Int)

  def highlightCell(highlight: Set[Coordinates])(coordinates: Coordinates, cell: Int): String =
    if (highlight.contains(coordinates)) {
      s"${Console.GREEN_B}${Console.BOLD}$cell${Console.RESET}"
    } else {
      s"${Console.YELLOW}$cell${Console.RESET}"
    }

  override def formatCell(coordinates: Coordinates, t: Int): String =
    highlightCell(minimalPathCoords)(coordinates, t)

  private val entryPoint: IndexedCell[Int] = {
    val coordinates = Coordinates(X(0), Y(0))
    IndexedCell(coordinates, values(coordinates))
  }

  private val exit: IndexedCell[Int] = {
    val coordinates = Coordinates(xMax, yMax)
    IndexedCell(coordinates, values(coordinates))
  }

  lazy val (minimalRisk, minimalPath): (Long, List[IdxVal]) = {
    @tailrec
    def shortestPath(
        current: Coordinates = entryPoint.coordinates,
        paths: Map[Coordinates, (Long, List[IdxVal])] = Map(entryPoint.coordinates -> (0L, List(entryPoint.coordinates -> entryPoint.value))),
        seen: Set[Coordinates] = Set(entryPoint.coordinates),
    ): (Long, List[IdxVal]) =
      if (current == exit.coordinates) {
        paths(current)
      } else {
        if (seen.size % (values.size / 10) == 0) {
          println(
            makeVisual(highlightCell(seen))
          )
        }

        val (weightToCurrent, pathToCurrent) = paths(current)

        val newPaths = adjacent(current, Neighbour.base)
          .filterNot { case (coordinates, _) => seen.contains(coordinates) }
          .foldLeft(
            paths.filterNot { case (coordinates, _) => seen.contains(coordinates) }
          ) { case (newPaths, newHead @ (coordinates, weight)) =>
            val newWeight = weightToCurrent + weight
            val newPath   = newHead +: pathToCurrent

            newPaths.updatedWith(coordinates) {
              case Some(existing @ (existingWeight, _)) if newWeight >= existingWeight => Some(existing)
              case _                                                                   => Some(newWeight -> newPath)
            }
          }

        val (newHead, _) = newPaths.iterator
          .filterNot { case (coordinates, _) => seen.contains(coordinates) }
          .minBy { case (_, (weight, _)) => weight }

        shortestPath(
          current = newHead,
          paths = newPaths,
          seen = seen ++ Set(newHead),
        )
      }

    shortestPath()
  }

  private lazy val minimalPathCoords: Set[Coordinates] =
    minimalPath.map(_._1).toSet
}

object Ceiling extends Grid2DCompanion[Int] {
  def apply(values: List[List[Int]]): Ceiling =
    Ceiling(makeIndexedMap(values))
}
