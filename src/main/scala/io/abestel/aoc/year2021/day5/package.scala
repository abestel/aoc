package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day5 {
  def dataStream[F[_]: Files: Sync](
      expand: (Pos, Pos) => List[Pos]
  ): F[Map[Pos, Int]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day5.txt")
      .map(_.split(" -> |,").map(_.toInt))
      .collect { case Array(x1, y1, x2, y2) => Pos(x1, y1) -> Pos(x2, y2) }
      .flatMap { case (pos1, pos2) => Stream.emits(expand(pos1, pos2)) }
      .fold(Map.empty[Pos, Int]) { case (theMap, (pos)) =>
        theMap.updatedWith(pos)(_.fold(1)(_ + 1).some)
      }
      .compile
      .lastOrError
}
