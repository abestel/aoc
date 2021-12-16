package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day3 {
  def dataStream[F[_]: Files: Sync]: Stream[F, List[Bit]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day3.txt")
      .evalMap(_.toList.traverse(Bit.parse(_)))
}
