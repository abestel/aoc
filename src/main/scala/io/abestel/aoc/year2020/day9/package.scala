package io.abestel.aoc.year2020

import cats.effect.Sync
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day9 {
  def dataStream[F[_]: Files: Sync]: F[List[Long]] =
    Fs2FilesExt
      .resourceStream("2020/day9.txt")
      .evalMap(_.toLongF[F])
      .compile
      .toList

  def invalid[F[_]: Sync](data: List[Long]): F[Long] =
    Sync[F].fromOption(
      data.sliding(26).collectFirst { case chunk if findInvalid(chunk) => chunk.last },
      new IllegalStateException("An invalid number should be present"),
    )

  def findInvalid(chunk: List[Long]): Boolean = {
    val current = chunk.last
    val toSum   = chunk.dropRight(1).toSet
    !toSum.exists(first => toSum.contains(current - first))
  }
}
