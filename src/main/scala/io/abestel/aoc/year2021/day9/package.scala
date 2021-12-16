package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day9 {
  def data[F[_]: Files: Sync]: F[World] =
    Fs2FilesExt
      .resourceStream[F]("2021/day9.txt")
      .evalMap(
        _.to(List).traverse(_.toString.toIntF[F])
      )
      .compile
      .toList
      .map(World(_))
}
