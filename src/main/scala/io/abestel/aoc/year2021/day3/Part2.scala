package io.abestel.aoc.year2021.day3

import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  def loop(
      values: List[List[Bit]]
  )(
      filter: (Int, Int) => Bit
  ): Option[Long] = {
    @tailrec
    def go(
        remaining: List[List[Bit]],
        pos: Int = 0,
    ): Option[List[Bit]] = {
      remaining match {
        case head :: Nil => Some(head)
        case Nil         => None

        case _ =>
          val (zeros, ones) = remaining.flatMap(_.lift(pos)).partition(_ == Bit.Zero)
          val bitToFilter   = filter(zeros.length, ones.length)
          val newRemaining  = remaining.filter(bits => bits.lift(pos).contains(bitToFilter))

          go(
            remaining = newRemaining,
            pos = pos + 1,
          )
      }
    }

    go(values).map(
      _.reverse
        .foldLeft(
          (
            /*power of 2*/ 1L,
            /*value*/ 0L,
          )
        ) {
          case ((pow, value), Bit.One)  => (pow * 2, value + pow)
          case ((pow, value), Bit.Zero) => (pow * 2, value)
        }
        ._2
    )
  }

  override def run: IO[Unit] =
    dataStream[IO].compile.toList.map { values =>
      val oxygen = loop(values) { case (numberOfZeros, numberOfOnes) =>
        if (numberOfOnes >= numberOfZeros) {
          Bit.One
        } else {
          Bit.Zero
        }
      }

      val co2 = loop(values) { case (numberOfZeros, numberOfOnes) =>
        if (numberOfZeros <= numberOfOnes) {
          Bit.Zero
        } else {
          Bit.One
        }
      }

      println {
        (oxygen, co2).tupled.map { case (oxygen, co2) =>
          oxygen * co2
        }
      }
    }
}
