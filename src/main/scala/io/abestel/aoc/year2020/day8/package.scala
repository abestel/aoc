package io.abestel.aoc.year2020

import cats.effect.Sync
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

import scala.annotation.tailrec

package object day8 {
  def dataStream[F[_]: Files: Sync]: F[List[Command]] =
    Fs2FilesExt
      .resourceStream("2020/day8.txt")
      .evalMap(Command.parse[F])
      .compile
      .toList

  def runLoop(
      commands: List[Command]
  ): Result = {
    @tailrec
    def go(
        commandIndex: Int = 0,
        accumulator: Int = 0,
        stack: Set[Int] = Set.empty,
    ): Result =
      if (stack contains commandIndex) {
        Result.InfiniteLoop(accumulator)
      } else {
        commands.lift(commandIndex) match {
          case None => Result.NormalExit(accumulator)
          case Some(command) =>
            command match {
              case Command.NoOp(_)           => go(commandIndex + 1, accumulator, stack + commandIndex)
              case Command.Accumulate(delta) => go(commandIndex + 1, accumulator + delta, stack + commandIndex)
              case Command.Jump(delta)       => go(commandIndex + delta, accumulator, stack + commandIndex)
            }
        }
      }

    go()
  }
}
