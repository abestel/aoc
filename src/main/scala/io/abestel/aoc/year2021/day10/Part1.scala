package io.abestel.aoc.year2021.day10

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .map(SyntaxTree.parser)
      .debug[SyntaxTree.Error Either SyntaxTree](debugSyntaxTreeParser)
      .collect { case Left(SyntaxTree.Error.SyntaxError(_, invalidCharacter, _, _)) =>
        invalidCharacter match {
          case Character.`)` => 3L
          case Character.`]` => 57L
          case Character.`}` => 1197L
          case Character.`>` => 25137L
          case _             => 0L
        }
      }
      .foldMonoid
      .compile
      .lastOrError
      .map(println(_))
}
