package io.abestel.aoc.year2020.day3

import cats.ApplicativeError

sealed trait Environment

object Environment {
  case object Tree extends Environment
  case object Open extends Environment

  def parse[F[_]](char: Char)(implicit F: ApplicativeError[F, Throwable]): F[Environment] =
    char match {
      case '#' => F.pure(Tree)
      case '.' => F.pure(Open)
      case _   => F.raiseError(new IllegalArgumentException(s"'$char' is not a valid environment"))
    }
}
