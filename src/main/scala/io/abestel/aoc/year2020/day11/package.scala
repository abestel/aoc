package io.abestel.aoc.year2020

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day11 {
  def data[F[_]: Files: Sync]: F[SeatMap] =
    Fs2FilesExt
      .resourceStream[F]("2020/day11.txt")
      .evalMap(
        _.toList.traverse(_.toString.toEnum[F, Element](Element))
      )
      .compile
      .toList
      .map(SeatMap(_))
}
