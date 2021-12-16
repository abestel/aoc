package io.abestel.aoc.year2020

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.year2020.day5.Strategy.{Split, StrategyOps}

import scala.annotation.tailrec

package object day5 {
  def dataStream[F[_]: Files: Sync]: Stream[F, Seat] =
    Fs2FilesExt
      .resourceStream("2020/day5.txt")
      .evalMap { boardingString =>
        for {
          rows    <- boardingString.take(7).toList.traverse(RowIndex.parse[F])
          columns <- boardingString.slice(7, 10).toList.traverse(ColumnIndex.parse[F])
        } yield BoardingPass(columns, rows)
      }
      .map { case BoardingPass(columns, rows) =>
        Seat(
          row = extractIndex(rows),
          column = extractIndex(columns),
        )
      }

  def extractIndex[T: Strategy](
      stack: List[T]
  ): Int = {
    @tailrec
    def go(
        remaining: List[T],
        lowerBound: Int,
        upperBound: Int,
    ): Int =
      remaining match {
        case head :: tail =>
          head.split match {
            case Split.LowerHalf =>
              go(
                remaining = tail,
                lowerBound = lowerBound,
                upperBound = lowerBound + (upperBound - lowerBound) / 2,
              )

            case Split.UpperHalf =>
              go(
                remaining = tail,
                lowerBound = lowerBound + (upperBound - lowerBound) / 2 + 1,
                upperBound = upperBound,
              )
          }

        case Nil => lowerBound
      }

    go(
      remaining = stack,
      lowerBound = 0,
      upperBound = Math.pow(2d, stack.length.toDouble).toInt - 1,
    )
  }
}
