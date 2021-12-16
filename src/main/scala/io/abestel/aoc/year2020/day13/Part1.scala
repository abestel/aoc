package io.abestel.aoc.year2020.day13

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { case (current, buses) =>
      val (busId, time) = buses
        .to(LazyList)
        .map { case (busId, _) =>
          @tailrec
          def findFirstTime(currentTimestamp: Long = current): Long =
            if (currentTimestamp % busId == 0) {
              currentTimestamp
            } else {
              findFirstTime(currentTimestamp + 1)
            }

          busId -> findFirstTime()
        }
        .reduce[(Long, Long)] { case ((bus1, time1), (bus2, time2)) =>
          if (time1 <= time2) {
            (bus1, time1)
          } else {
            (bus2, time2)
          }
        }

      println(busId * (time - current))
    }
}
