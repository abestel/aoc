package io.abestel.aoc.utils.grid

import cats.implicits._

import scala.annotation.tailrec

trait Grid2D[T] {
  def values: Map[Coordinates, T]

  lazy val xMin: X = values.keySet.map { case Coordinates(x, _) => x }.minBy(_.value)
  lazy val xMax: X = values.keySet.map { case Coordinates(x, _) => x }.maxBy(_.value)
  lazy val yMin: Y = values.keySet.map { case Coordinates(_, y) => y }.minBy(_.value)
  lazy val yMax: Y = values.keySet.map { case Coordinates(_, y) => y }.maxBy(_.value)

  lazy val xRange: List[X] = (xMin.value to xMax.value).map(X(_)).toList
  lazy val yRange: List[Y] = (yMin.value to yMax.value).map(Y(_)).toList

  def makeVisual(format: (Coordinates, T) => String): String =
    if (values.isEmpty) {
      "<empty>\n"
    } else {
      yRange
        .map { y =>
          xRange.map { x =>
            val coords = Coordinates(x, y)
            get(coords).fold(formatEmptyCell)(format(coords, _))
          }.mkString
        }
        .mkString("\n") + "\n"
    }

  lazy val visualize: String = makeVisual(formatCell)

  def formatEmptyCell: String = " "
  def formatCell(coordinates: Coordinates, t: T): String

  def get(coordinates: Coordinates): Option[T] =
    values.get(coordinates)

  def adjacent(coordinates: Coordinates, neighbours: List[Neighbour]): LazyList[(Coordinates, T)] =
    neighbours.to(LazyList).flatMap { case Neighbour(deltaX, deltaY) =>
      val neighbourCoordinates = coordinates.transform(deltaX, deltaY)
      get(neighbourCoordinates).map(neighbourCoordinates -> _)
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
