package io.abestel.aoc.year2020.day8

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    dataStream[IO]
      .map { commands =>
        commands
          .to(LazyList)
          .zipWithIndex
          .collect {
            case (Command.NoOp(delta), commandIndex) => commands.updated(commandIndex, Command.Jump(delta))
            case (Command.Jump(delta), commandIndex) => commands.updated(commandIndex, Command.NoOp(delta))
          }
          .map(runLoop)
          .collectFirst { case Result.NormalExit(accumulator) =>
            accumulator
          }
      }
      .map(println)
}
