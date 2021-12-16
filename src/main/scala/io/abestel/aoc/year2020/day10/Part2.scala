package io.abestel.aoc.year2020.day10

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO].map { adapters =>
      println(arrangements(adapters))
    }

  private def arrangements(adapterList: List[Long]): Long = {
    val maxAdapter = adapterList.max

    @tailrec
    def go(
        adapters: List[Long] = adapterList.sorted,
        seen: Map[Long, Long] = Map(0L -> 1L).withDefaultValue(0),
    ): Long =
      adapters match {
        case Nil => seen(maxAdapter)
        case jolt :: newJolts =>
          val joltValue = seen(jolt - 3) + seen(jolt - 2) + seen(jolt - 1)
          val newPrevs  = seen + (jolt -> joltValue)
          go(newJolts, newPrevs)
      }

    go()
  }
}
