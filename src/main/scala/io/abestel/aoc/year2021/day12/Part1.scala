package io.abestel.aoc.year2021.day12

import cats.effect.{IO, IOApp}
import cats.implicits.toShow

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map(
        paths { case (nel, newCaves) =>
          newCaves
            .collect {
              case nextCave if !nel.exists(_ == nextCave) || nextCave.isBig =>
                nextCave :: nel
            }
        }
      )
      .map { allPaths =>
        println {
          allPaths
            .map(
              _.toList
                .map(_.show)
                .mkString(" -> ")
            )
            .zipWithIndex
            .map(_.swap)
            .mkString("\n")
        }
      }
}
