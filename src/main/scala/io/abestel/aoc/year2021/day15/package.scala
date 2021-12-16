package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day15 {
  def data[F[_]: Files: Sync]: F[Ceiling] =
    Fs2FilesExt
      .resourceStream[F]("2021/day15.txt")
      .evalMap(_.toList.traverse(_.toString.toIntF[F]))
      .compile
      .toList
      .map(Ceiling(_))
}
