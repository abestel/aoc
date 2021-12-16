package io.abestel.aoc.year2020

import cats.effect.Sync
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day7 {
  type BagName = String

  def dataStream[F[_]: Files: Sync]: Stream[F, (BagName, BagRule)] =
    Fs2FilesExt
      .resourceStream("2020/day7.txt")
      .evalMap(BagRule.parse[F])
}
