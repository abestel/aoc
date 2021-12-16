package io.abestel.aoc.year2021

import cats.ApplicativeError
import cats.effect.Sync
import cats.effect.std.Console
import cats.implicits._
import fs2.{Pipe, Stream}
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day1 {
  def dataStream[F[_]: Files: Sync]: Stream[F, Long] =
    Fs2FilesExt
      .resourceStream("2021/day1.txt")
      .evalMap { rawLong =>
        ApplicativeError[F, Throwable].fromOption(
          rawLong.toLongOption,
          new IllegalArgumentException(s"'$rawLong' is not a valid Long"),
        )
      }

  def transformAndCount[F[_]: Console: Files: Sync](pipe: Pipe[F, Long, Long]): F[Unit] =
    dataStream[F]
      .through(pipe)
      .zipWithPrevious
      .filter {
        case (Some(previous), current) if previous < current => true
        case _                                               => false
      }
      .compile
      .count
      .flatMap(Console[F].println)

}
