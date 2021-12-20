package io.abestel.aoc.year2021

import cats.ApplicativeError
import cats.effect.Sync
import cats.syntax.either._
import fs2.io.file.Files
import fs2.Stream
import io.abestel.aoc.utils.Fs2FilesExt

package object day18 {
  def data[F[_]: Files: Sync]: Stream[F, Snailfish] =
    Fs2FilesExt
      .resourceStream[F]("2021/day18.txt")
      .evalMap { raw =>
        ApplicativeError[F, Throwable].fromEither(
          Snailfish.parser
            .parse(raw)
            .bimap(
              error => new IllegalArgumentException(s"Error while parsing '$raw': $error"),
              { case (_, snailfish) => snailfish },
            )
        )
      }
}
