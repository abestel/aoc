package io.abestel.aoc.year2021.day3

import cats.{Applicative, ApplicativeError}
import cats.effect.Sync

sealed trait Bit

object Bit {
  case object One  extends Bit
  case object Zero extends Bit

  def parse[F[_]: Sync](bit: Char): F[Bit] =
    bit match {
      case '0'   => Applicative[F].pure(Zero)
      case '1'   => Applicative[F].pure(One)
      case other => ApplicativeError[F, Throwable].raiseError(new IllegalArgumentException(s"Character '$other' is neither '0' nor '1'"))
    }
}
