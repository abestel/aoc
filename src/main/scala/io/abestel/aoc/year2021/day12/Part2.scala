package io.abestel.aoc.year2021.day12

import cats.effect.{IO, IOApp}
import cats.implicits.toShow

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map(
        paths { case (nel, newCaves) =>
          lazy val visited = nel.toList.groupMapReduce(identity)(_ => 1)(_ + _) withDefaultValue 0
          lazy val anySmallCaveVisitedTwice = visited.iterator.exists {
            case (Cave.Small(_), count) => count > 1
            case _                      => false
          }

          newCaves
            .filter {
              case Cave.Start             => false
              case Cave.Big(_) | Cave.End => true
              case cave @ Cave.Small(_) =>
                visited(cave) match {
                  case 0 => true
                  case 1 => !anySmallCaveVisitedTwice
                  case _ => false
                }
            }
            .map(_ :: nel)
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
