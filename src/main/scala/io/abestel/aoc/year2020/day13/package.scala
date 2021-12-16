package io.abestel.aoc.year2020

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day13 {

  def data[F[_]: Files: Sync]: F[(Long, List[(Long, Long)])] = {
    val stream = Fs2FilesExt.resourceStream[F]("2020/day13.txt")

    val currentTimestamp = stream.head.evalMap(_.toLongF[F]).compile.lastOrError
    val buses = stream.tail
      .evalMap(
        _.split(",").toList.zipWithIndex
          .traverse[F, Option[(Long, Long)]] {
            case ("x", _)       => Sync[F].pure(None)
            case (other, index) => other.toLongF[F].map(busId => Some(busId -> index.toLong))
          }
          .map(_.flatten)
      )
      .compile
      .lastOrError

    (
      currentTimestamp,
      buses,
    ).tupled
  }
}
