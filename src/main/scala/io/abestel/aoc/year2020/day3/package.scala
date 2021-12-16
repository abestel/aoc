package io.abestel.aoc.year2020

import cats.effect.Sync
import cats.syntax.traverse._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day3 {
  def dataStream[F[_]: Files: Sync]: Stream[F, LineMap] =
    Fs2FilesExt
      .resourceStream("2020/day3.txt")
      .evalMap(
        _.toList.traverse(Environment.parse[F])
      )
      .map(LineMap)

  def runSlope[F[_]: Files: Sync](downStep: Int, rightStep: Int): F[Long] =
    dataStream[F].zipWithIndex
      .collect {
        case (lineMap, index) if index % downStep == 0 =>
          lineMap
      }
      .zipWithIndex
      .filter { case (lineMap, index) =>
        lineMap.at(index * rightStep) == Environment.Tree
      }
      .compile
      .count
}
