package io.abestel.aoc.year2021.day3

import cats.Semigroup
import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import cats.implicits._

object Part1 extends IOApp.Simple {
  case class BitPos(
      numberOfZero: Int,
      numberOfOne: Int,
  )

  object BitPos {
    def zero: BitPos = BitPos(numberOfZero = 1, numberOfOne = 0)
    def one: BitPos  = BitPos(numberOfZero = 0, numberOfOne = 1)

    implicit val bitCountSemigroup: Semigroup[BitPos] = { case (bc1, bc2) =>
      BitPos(
        numberOfZero = bc1.numberOfZero + bc2.numberOfZero,
        numberOfOne = bc1.numberOfOne + bc2.numberOfOne,
      )
    }
  }

  case class Bits(
      value: List[BitPos]
  )

  object Bits {
    def fromBitList(bits: List[Bit]): Bits =
      Bits(
        bits.map {
          case Bit.One  => BitPos.one
          case Bit.Zero => BitPos.zero
        }
      )

    implicit val bitsSemiGroup: Semigroup[Bits] = { case (Bits(bits1), Bits(bits2)) =>
      Bits {
        bits1.zip(bits2).map { case (bp1, bp2) =>
          bp1 combine bp2
        }
      }
    }
  }

  override def run: IO[Unit] =
    dataStream[IO]
      .map(Bits.fromBitList)
      .reduceSemigroup
      .compile
      .lastOrError
      .map { case Bits(bits) =>
        bits.reverse.foldLeft(
          (
            /*power of 2*/ 1L,
            /*gamma*/ 0L,
            /*epsilon*/ 0L,
          )
        ) { case ((pow, gamma, epsilon), BitPos(numberOfZero, numberOfOne)) =>
          if (numberOfZero >= numberOfOne) {
            (
              pow * 2,
              gamma,
              epsilon + pow,
            )
          } else {
            (
              pow * 2,
              gamma + pow,
              epsilon,
            )
          }
        }
      }
      .flatMap { case (_, gamma, epsilon) =>
        Console[IO].println(gamma * epsilon)
      }
}
