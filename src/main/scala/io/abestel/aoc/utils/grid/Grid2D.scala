package io.abestel.aoc.utils.grid

import cats.implicits._

import scala.annotation.tailrec

trait Grid2D[T] {
  def values: Map[Coordinates, T]

  lazy val xMax: X = values.keySet.map { case Coordinates(x, _) => x }.maxBy(_.value)
  lazy val yMax: Y = values.keySet.map { case Coordinates(_, y) => y }.maxBy(_.value)

  lazy val xRange: List[X] = (0 to xMax.value).map(X(_)).toList
  lazy val yRange: List[Y] = (0 to yMax.value).map(Y(_)).toList

  def makeVisual(format: (Coordinates, T) => String): String =
    if (values.isEmpty) {
      "<empty>\n"
    } else {
      yRange
        .map { y =>
          xRange.map { x =>
            val coords = Coordinates(x, y)

            values
              .get(coords)
              .fold(formatEmptyCell)(format(coords, _))
          }.mkString
        }
        .mkString("\n") + "\n"
    }

  lazy val visualize: String = makeVisual(formatCell)

  def formatEmptyCell: String = " "
  def formatCell(coordinates: Coordinates, t: T): String

  def adjacent(coordinates: Coordinates, neighbours: List[Neighbour]): LazyList[(Coordinates, T)] =
    neighbours.to(LazyList).flatMap { case Neighbour(deltaX, deltaY) =>
      val neighbourCoordinates = coordinates.transform(deltaX, deltaY)
      values.get(neighbourCoordinates).map(neighbourCoordinates -> _)
    }

  def findFirst(coordinates: Coordinates, neighbours: List[Neighbour])(filter: T => Boolean): LazyList[(Coordinates, T)] = {
    @tailrec
    def findFirstLoop(coordinates: Coordinates, deltaX: Int, deltaY: Int): Option[(Coordinates, T)] = {
      val newCoordinates = coordinates.transform(deltaX, deltaY)
      values.get(newCoordinates) match {
        case None => None
        case Some(value) =>
          if (filter(value)) {
            Some((newCoordinates, value))
          } else {
            findFirstLoop(newCoordinates, deltaX, deltaY)
          }
      }
    }

    neighbours.to(LazyList).flatMap { case Neighbour(deltaX, deltaY) =>
      findFirstLoop(coordinates, deltaX, deltaY)
    }
  }

}
