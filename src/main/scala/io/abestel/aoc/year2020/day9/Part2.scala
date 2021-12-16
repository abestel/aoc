package io.abestel.aoc.year2020.day9

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      data          <- dataStream[IO]
      invalidNumber <- invalid[IO](data)
      (smallest, largest) <- IO.fromOption(
        loop(data, invalidNumber)
      )(
        new IllegalStateException(s"Could not find a sequence equal $invalidNumber")
      )
    } yield println(smallest + largest)

  def loop(numbers: List[Long], target: Long): Option[(Long, Long)] = {
    @tailrec
    def slidingSum(
        remaining: List[Long],
        currentSum: Long = 0L,
        smallest: Long = Long.MaxValue,
        largest: Long = Long.MinValue,
    ): Option[(Long, Long)] =
      if (currentSum == target) {
        Some(smallest -> largest)
      } else if (currentSum > target) {
        None
      } else {
        remaining match {
          case Nil => None
          case current :: tail =>
            slidingSum(
              currentSum = currentSum + current,
              smallest = smallest min current,
              largest = largest max current,
              remaining = tail,
            )
        }
      }

    numbers.indices
      .to(LazyList)
      .map(index => slidingSum(numbers.drop(index)))
      .collectFirst { case Some(value) => value }
  }
}
