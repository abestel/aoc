package io.abestel.aoc.year2021.day20

import cats.data.State
import cats.implicits._
import io.abestel.aoc.utils.grid._

import scala.collection.immutable.BitSet

case class Image(
    values: Map[Coordinates, Pixel],
    outer: Pixel,
) extends Grid2D[Pixel] {

  override def formatCell(coordinates: Coordinates, t: Pixel): String =
    t match {
      case Pixel.Dark  => formatEmptyCell
      case Pixel.Light => s"${Console.GREEN_B}#${Console.RESET}"
    }

  override def formatEmptyCell: String = s"${Console.YELLOW}.${Console.RESET}"

  override def get(coordinates: Coordinates): Option[Pixel] =
    super.get(coordinates).orElse(Some(outer))
}

object Image extends Grid2DCompanion[Pixel] {
  def apply(pixels: List[List[Pixel]]): Image =
    Image(
      values = makeIndexedMap(pixels).filterNot(_._2 == Pixel.Dark),
      outer = Pixel.Dark,
    )

  def step(algo: BitSet): State[Image, Unit] =
    State.modify { image =>
      def pixelValue(bits: List[Pixel]): Pixel = {
        val isLight = algo(
          bits.reverse
            .foldLeft(
              (
                1,
                0,
              )
            ) { case ((powerOfTwo, result), current) =>
              current match {
                case Pixel.Dark  => (powerOfTwo * 2, result)
                case Pixel.Light => (powerOfTwo * 2, result + powerOfTwo)
              }
            }
            ._2
        )

        if (isLight) Pixel.Light
        else Pixel.Dark
      }

      val newOuter = pixelValue((0 until 9).map(_ => image.outer).toList)

      val newPixels = Iterator.range(image.yMin.value - 5, image.yMax.value + 6)
        .map(Y(_))
        .flatMap { y =>
          Iterator.range(image.xMin.value - 5, image.xMax.value + 6).map(X(_)).map { x =>
            val coord      = Coordinates(x, y)
            val current    = image.get(coord).map(coord -> _)
            val neighbours = image.adjacent(coord, Neighbour.all)

            val newPixel = pixelValue(
              (neighbours ++ current)
                .sortBy(_._1)
                .map(_._2)
                .toList
            )

            coord -> newPixel
          }
        }
        .filterNot(_._2 == newOuter)
        .toMap

      Image(newPixels, newOuter)
    }

  def steps(algo: BitSet)(n: Int): State[Image, Unit] =
    (0 until n).toList.traverse(_ => step(algo)).map(_ => ())
}
