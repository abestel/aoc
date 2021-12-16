package io.abestel.aoc.year2020.day5

import cats.ApplicativeError

sealed trait ColumnIndex

object ColumnIndex {
  case object Left  extends ColumnIndex
  case object Right extends ColumnIndex

  implicit val columnStrategy: Strategy[ColumnIndex] = {
    case ColumnIndex.Left  => Strategy.Split.LowerHalf
    case ColumnIndex.Right => Strategy.Split.UpperHalf
  }

  def parse[F[_]](char: Char)(implicit F: ApplicativeError[F, Throwable]): F[ColumnIndex] =
    char match {
      case 'L' => F.pure(Left)
      case 'R' => F.pure(Right)
      case _   => F.raiseError(new IllegalArgumentException(s"'$char' is not a valid column"))
    }
}
