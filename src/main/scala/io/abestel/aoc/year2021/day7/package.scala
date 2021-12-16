package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day7 {
  def dataStream[F[_]: Files: Sync]: F[List[Int]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day7.txt")
      .head
      .evalMap(_.split(",").toList.traverse(_.toIntF[F]))
      .compile
      .lastOrError
}
