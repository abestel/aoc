package io.abestel.aoc.year2021

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.geom.Pos3

import scala.collection.immutable.SortedSet

package object day19 {
  def data[F[_]: Files: Sync]: F[List[Scanner]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day19.txt")
      .groupAdjacentBy(_.nonEmpty)
      .collect { case (nonEmpty, chunks) if nonEmpty => chunks }
      .evalMap { rawScanner =>
        rawScanner.foldM[F, SortedSet[Pos3]](SortedSet.empty[Pos3]) { case (probes, rawLine) =>
          rawLine match {
            case s"--- scanner $_ ---" =>
              Sync[F].pure(probes)

            case s"$xr,$yr,$zr" =>
              Sync[F].fromOption(
                for {
                  x <- xr.toIntOption
                  y <- yr.toIntOption
                  z <- zr.toIntOption
                } yield probes + Pos3(x, y, z),
                new IllegalArgumentException(s"'$rawLine' is not a valid set of coordinates"),
              )

            case _ =>
              Sync[F].raiseError(new IllegalArgumentException(s"'$rawLine' is not valid"))
          }
        }
      }
      .evalMap(probes =>
        Sync[F].fromOption(
          NonEmptySet.fromSet(probes),
          new IllegalArgumentException(s"Parsed an empty set of probes"),
        )
      )
      .compile
      .toList
      .flatMap(scanners =>
        Sync[F].fromOption(
          scanners.toNel,
          new IllegalArgumentException(s"Parsed an empty set of scanners"),
        )
      )
      .map(Scanner.align)
}
