package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._
import io.abestel.aoc.utils.grid._

package object day13 {
  def data[F[_]: Files: Sync](filename: String): F[Transparent] =
    Fs2FilesExt
      .resourceStream[F](s"2021/${filename}.txt")
      .filter(_.nonEmpty)
      .evalScan(Transparent.empty) { case (transparent, line) =>
        println(line)
        line match {
          case s"$coord1,$coord2" =>
            (
              coord1.trim.toIntF[F],
              coord2.trim.toIntF[F],
            ).tupled.map { case (x, y) =>
              transparent.mark(X(x), Y(y))
            }

          case s"fold along y=$y" =>
            y.trim
              .toIntF[F]
              .map(Y(_))
              .map { yFold =>
                val withFold = transparent.prepareHorizontalFold(yFold)
                println(withFold.visualize)
                val folded = withFold.processHorizontalFold(yFold)
                println(folded.visualize)
                folded
              }

          case s"fold along x=$x" =>
            x.trim
              .toIntF[F]
              .map(X(_))
              .map { xFold =>
                val withFold = transparent.prepareVerticalFold(xFold)
                println(withFold.visualize)
                val folded = withFold.processVerticalFold(xFold)
                println(folded.visualize)
                folded
              }

          case _ =>
            Sync[F].raiseError(
              new IllegalArgumentException(s"Wrong input '$line'")
            )
        }
      }
      .compile
      .lastOrError

}
