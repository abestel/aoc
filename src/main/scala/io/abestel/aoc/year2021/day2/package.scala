package io.abestel.aoc.year2021

import cats.effect.Sync
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day2 {
  def dataStream[F[_]: Files: Sync]: Stream[F, Command] =
    Fs2FilesExt
      .resourceStream[F]("2021/day2.txt")
      .filter(_.nonEmpty)
      .evalMap(Command.parse[F])

  def stream[F[_]: Files: Sync, T: Commandable]: F[T] =
    dataStream[F]
      .fold(Commandable[T].zero)(Commandable[T].accept)
      .compile
      .lastOrError
}
