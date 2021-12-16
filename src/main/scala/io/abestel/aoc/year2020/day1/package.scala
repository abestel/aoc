package io.abestel.aoc.year2020

import cats.ApplicativeError
import cats.effect.Sync
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day1 {
  def dataStream[F[_]: Files: Sync]: Stream[F, Long] =
    Fs2FilesExt
      .resourceStream("2020/day1.txt")
      .evalMap { rawLong =>
        ApplicativeError[F, Throwable].fromOption(
          rawLong.toLongOption,
          new IllegalArgumentException(s"'$rawLong' is not a valid Long"),
        )
      }
}
