package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._
import io.abestel.aoc.year2021.day10.SyntaxTree.Error

package object day10 {
  def data[F[_]: Files: Sync]: Stream[F, List[Character]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day10.txt")
      .evalMap(
        _.toList.traverse(
          _.toString.toEnum[F, Character](Character)
        )
      )

  def debugSyntaxTreeParser(result: SyntaxTree.Error Either SyntaxTree): String = {
    result match {
      case Right(syntaxTree) => s"${Console.GREEN}OK${Console.RESET} ${syntaxTree.show}"
      case Left(error) =>
        val description = error match {
          case Error.SyntaxError(program, _, atPosition, _) =>
            val highlight = program.zipWithIndex.map { case (character, index) =>
              if (index == atPosition) {
                s"${Console.RED}${Console.BOLD}$character${Console.RESET}"
              } else {
                character.entryName
              }
            }.mkString

            val description = error.getMessage

            s"$highlight - $description"

          case Error.Incomplete(program, _, openedAt, _, _) =>
            val highlight = program.zipWithIndex.map { case (character, index) =>
              if (index == openedAt) {
                s"${Console.RED}${Console.BOLD}$character${Console.RESET}"
              } else {
                character.entryName
              }
            }.mkString + s"${Console.RED_B} ${Console.RESET}"

            val description = error.getMessage

            s"$highlight - $description"
        }

        s"${Console.RED}NOK${Console.RESET} $description"
    }
  }
}
