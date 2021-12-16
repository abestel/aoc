package io.abestel.aoc.year2020

import cats.ApplicativeError
import cats.effect.Sync
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day2 {
  private val lineExtractor = "(?<minBound>[0-9]+)-(?<maxBound>[0-9]+) (?<pattern>.+): (?<password>.+)".r

  def dataStream[F[_]: Files: Sync]: Stream[F, (Rule, String)] =
    Fs2FilesExt
      .resourceStream("2020/day2.txt")
      .evalMap { rawLine =>
        ApplicativeError[F, Throwable].fromOption(
          rawLine match {
            case lineExtractor(rawMinBound, rawMaxBound, pattern, password) =>
              for {
                minBound  <- rawMinBound.toIntOption
                maxBound  <- rawMaxBound.toIntOption
                character <- pattern.headOption
              } yield Rule(minBound, maxBound, character) -> password

            case _ =>
              None
          },
          new IllegalArgumentException(s"Line '$rawLine' is not valid"),
        )
      }
}
