package io.abestel.aoc.year2021

import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day6 {
  def dataStream[F[_]: Files]: Stream[F, FishWorld] =
    Fs2FilesExt
      .resourceStream[F]("2021/day6.txt")
      .head
      .map(
        _.split(",").map(_.toInt).toList
      )
      .map(FishWorld.fromAgeList)
}
