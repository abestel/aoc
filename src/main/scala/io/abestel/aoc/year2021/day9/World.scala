package io.abestel.aoc.year2021.day9

import io.abestel.aoc.utils.grid._

import scala.annotation.tailrec

case class World(
    values: Map[Coordinates, Int]
) extends Grid2D[Int] {
  lazy val lowPoints: Set[(Coordinates, Int)] =
    values.iterator.filter { case (coordinates, height) =>
      adjacent(coordinates, Neighbour.base)
        .forall { case (_, value) => value > height }
    }.toSet

  lazy val lowPointsCoords: Set[Coordinates] =
    lowPoints.map { case (coords, _) => coords }

  lazy val bassins: List[World.Bassin] = {
    @tailrec
    def expand(
        heads: List[(Coordinates, Int)],
        result: Set[(Coordinates, Int)] = Set.empty,
    ): World.Bassin = {
      heads match {
        case Nil => World.Bassin(result)

        case _ =>
          val newHeads = (for {
            (headCoordinates, headHeight)           <- heads
            (neighbourCoordinates, neighbourHeight) <- adjacent(headCoordinates, Neighbour.base)
            if neighbourHeight > headHeight && neighbourHeight != 9
          } yield (neighbourCoordinates, neighbourHeight)).distinct

          expand(
            heads = newHeads,
            result = result ++ heads,
          )
      }
    }

    lowPoints.toList.map(lowPoint => expand(List(lowPoint)))
  }

  private lazy val coordinateToColor: Map[Coordinates, String] = {
    val colors = List(
      Console.CYAN,
      Console.MAGENTA,
      Console.YELLOW,
      Console.BLUE,
      Console.RED,
      Console.WHITE,
    )

    bassins.zipWithIndex.flatMap { case (bassin, bassinIndex) =>
      val bassinColor = colors(bassinIndex % colors.size)
      bassin.cells.map { case (coords, _) =>
        coords -> bassinColor
      }
    }.toMap
  }

  override def formatCell(coordinates: Coordinates, height: Int): String =
    if (lowPointsCoords.contains(coordinates)) {
      s"${Console.GREEN_B}$height${Console.RESET}"
    } else if (height == 9) {
      " "
    } else {
      s"${coordinateToColor(coordinates)}$height${Console.RESET}"
    }
}

object World extends Grid2DCompanion[Int] {
  def apply(heights: List[List[Int]]): World =
    World(makeIndexedMap(heights))

  case class Bassin(
      cells: Set[(Coordinates, Int)]
  )
}
