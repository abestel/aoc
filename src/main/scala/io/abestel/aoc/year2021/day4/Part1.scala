package io.abestel.aoc.year2021.day4

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import cats.implicits._

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .reduce[(BingoCard, BingoState)] { case ((card1, state1), (card2, state2)) =>
        if (
          state1.numberDrawn < state2.numberDrawn ||
          (
            state1.numberDrawn == state2.numberDrawn &&
              card1.score(state1) > card2.score(state2)
          )
        ) {
          (card1, state1)
        } else {
          (card2, state2)
        }
      }
      .compile
      .lastOrError
      .flatMap { case (winningCard, winningState) =>
        Console[IO].println(winningCard.score(winningState))
      }
}
