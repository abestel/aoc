package io.abestel.aoc.year2021.day16

import cats.effect.{IO, IOApp}
import scodec.DecodeResult

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map(
      _.foreach { case (raw, DecodeResult(packet, remainder)) =>
        val remaainderStr =
          if (remainder.isEmpty) {
            "<empty>"
          } else {
            remainder.toBin
          }

        println(
          s"""============
            |${Console.BOLD}Raw value:     ${Console.RESET}${Console.YELLOW}$raw${Console.RESET}
            |${Console.BOLD}Total version: ${Console.RESET}${Console.BLUE}${packet.totalVersion}${Console.RESET}
            |${Console.BOLD}Total value:   ${Console.RESET}${Console.GREEN}${packet.value}${Console.RESET}
            |${Console.BOLD}Remainder  :   ${Console.RESET}${Console.YELLOW}${remaainderStr}${Console.RESET}
            |${packet.prettyPrint()}
            |============
            |""".stripMargin
        )
      }
    )
}
