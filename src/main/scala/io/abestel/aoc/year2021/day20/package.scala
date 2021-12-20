package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

import scala.collection.immutable.BitSet

package object day20 {
  def data[F[_]: Files: Sync]: F[(BitSet, Image)] = {
    val stream = Fs2FilesExt.resourceStream[F]("2021/day20.txt")

    val algo = stream.head
      .evalMap(
        _.toList.zipWithIndex.foldM(BitSet.empty) { case (bitset, (currentChar, index)) =>
          parsePixel(bitset.incl(index))(bitset.excl(index))(currentChar)
        }
      )
      .compile
      .lastOrError

    val pixels = stream.tail
      .filter(_.nonEmpty)
      .evalMap(
        _.toList.traverse(parsePixel[F, Pixel](Pixel.Light)(Pixel.Dark))
      )
      .compile
      .toList
      .map(Image(_))

    (algo, pixels).tupled
  }

  private def parsePixel[F[_]: Sync, T](onLight: => T)(onDark: => T)(currentChar: Char): F[T] =
    currentChar match {
      case '#' => Sync[F].pure(onLight)
      case '.' => Sync[F].pure(onDark)
      case _   => Sync[F].raiseError(new IllegalArgumentException(s"'$currentChar' is not valid"))
    }
}
