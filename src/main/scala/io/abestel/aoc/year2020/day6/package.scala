package io.abestel.aoc.year2020

import fs2.{Chunk, Stream}
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day6 {
  def dataStream[F[_]: Files]: Stream[F, Chunk[String]] =
    Fs2FilesExt
      .resourceStream("2020/day6.txt")
      .groupAdjacentBy(_.nonEmpty)
      .collect {
        case (nonEmpty, chunks) if nonEmpty =>
          chunks
      }
}
