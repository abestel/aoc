package io.abestel.aoc.year2020

import cats.effect.Sync
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day10 {
  def dataStream[F[_]: Files: Sync]: F[List[Long]] =
    Fs2FilesExt
      .resourceStream("2020/day10.txt")
      .evalMap(_.toLongF[F])
      .compile
      .toList
}
