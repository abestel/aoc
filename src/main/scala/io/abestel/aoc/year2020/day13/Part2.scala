package io.abestel.aoc.year2020.day13

import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      (_, buses) <- data[IO]
    } yield {
      println(buses)
      println {
        chineseRemainder(
          buses.map(_._1),
          buses.map { case (n, a) => n - a },
          0,
          buses.map(_._1).product,
        )
      }
    }

  @tailrec
  def chineseRemainder(ns: List[BigInt], as: List[BigInt], t: BigInt, lcm: BigInt): BigInt = {
    def mulInv(a: BigInt, b: BigInt): BigInt = {
      @tailrec
      def loop(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt =
        if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0)
        else x1

      if (b == 1) 1
      else {
        val x1 = loop(a, b, 0, 1)
        if (x1 < 0) x1 + b
        else x1
      }
    }

    if (ns.isEmpty) {
      t % lcm
    } else {
      val p = lcm / ns.head
      chineseRemainder(ns.tail, as.tail, t + as.head * mulInv(p, ns.head) * p, lcm)
    }
  }

}
