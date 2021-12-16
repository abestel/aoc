package io.abestel.aoc.year2021.day10

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map(SyntaxTree.parser)
      .debug[SyntaxTree.Error Either SyntaxTree](debugSyntaxTreeParser)
      .collect { case Left(SyntaxTree.Error.Incomplete(_, _, _, _, stack)) =>
        stack
          .collect { case opening: SyntaxTree.Opening[o, c] => opening.closer }
          .map {
            case Character.`)` => 1
            case Character.`]` => 2
            case Character.`}` => 3
            case Character.`>` => 4
          }
          .foldLeft(0L)(_ * 5 + _)
      }
      .compile
      .toList
      .map { scoreList =>
        scoreList.sorted.apply(scoreList.size / 2)
      }
      .map(println(_))
}
