package io.abestel.aoc.year2021.day4

import cats.{ApplicativeError, Traverse}
import cats.implicits._
import io.abestel.aoc.utils.errors._

case class BingoCard(
    lines: List[List[Int]]
) {
  def score(state: BingoState): Int =
    (for {
      lastDrawn <- state.lastDrawn.toList
      line      <- lines
      value     <- line
      if !state.contains(value)
    } yield value * lastDrawn).sum

  def hasBingo(state: BingoState): Boolean =
    (lines ++ lines.transpose).exists(
      _.forall(state.contains)
    )
}

object BingoCard {
  def parse[F[_], C[_]: Traverse](rawLines: C[String])(implicit F: ApplicativeError[F, Throwable]): F[BingoCard] =
    rawLines
      .traverse(
        _.split("\\s+").toList
          .filter(_.nonEmpty)
          .traverse(_.toIntF[F])
      )
      .map(_.toList)
      .map(BingoCard(_))
}
