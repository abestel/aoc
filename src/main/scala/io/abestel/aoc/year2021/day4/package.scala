package io.abestel.aoc.year2021

import cats.ApplicativeError
import cats.effect.Sync
import cats.implicits._
import fs2.{Chunk, Stream}
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

import scala.annotation.tailrec

package object day4 {
  def dataStream[F[_]: Files: Sync]: Stream[F, (BingoCard, BingoState)] = {
    val rawStream = Fs2FilesExt.resourceStream[F]("2021/day4.txt")

    val drawOrderStream = rawStream.head.evalMap(DrawOrder.parse[F])
    val cardsStream =
      rawStream.tail
        .dropWhile(_.isEmpty)
        .groupAdjacentBy(_.nonEmpty)
        .collect { case (nonEmpty, chunks) if nonEmpty => chunks }
        .evalMap(BingoCard.parse[F, Chunk])

    drawOrderStream.flatMap { drawOrder =>
      cardsStream.evalMap { card =>
        scoreCard[F](drawOrder, card).map(card -> _)
      }
    }
  }

  def scoreCard[F[_]](
      drawOrder: DrawOrder,
      bingoCard: BingoCard,
  )(implicit
      F: ApplicativeError[F, Throwable]
  ): F[BingoState] = {
    @tailrec
    def go(
        remainingNumbers: List[Int] = drawOrder.numbers,
        state: BingoState = BingoState.empty,
    ): Option[BingoState] =
      if (bingoCard.hasBingo(state)) {
        Some(state)
      } else {
        remainingNumbers match {
          case current :: newRemaining =>
            go(
              remainingNumbers = newRemaining,
              state = state + current,
            )

          case Nil =>
            None
        }
      }

    F.fromOption(
      go(),
      new IllegalStateException(s"The card $bingoCard never wins"),
    )
  }
}
