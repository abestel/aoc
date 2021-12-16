package io.abestel.aoc.year2020.day10

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO].map { adapters =>
      val sorted = adapters.sorted
      val all    = 0L +: sorted :+ (sorted.last + 3L)
      val diffCount = all
        .sliding(2)
        .collect { case firstAdapter :: secondAdapter :: Nil => secondAdapter - firstAdapter }
        .toList
        .groupMapReduce(identity)(_ => 1)(_ + _)

      val diff1 = diffCount(1)
      val diff3 = diffCount(3)
      println(diff1)
      println(diff3)
      println(diff1 * diff3)
    }
}
