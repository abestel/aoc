package io.abestel.aoc.year2020.day5

import cats.ApplicativeError

sealed trait RowIndex

object RowIndex {
  case object Front extends RowIndex
  case object Back  extends RowIndex

  def parse[F[_]](char: Char)(implicit F: ApplicativeError[F, Throwable]): F[RowIndex] =
    char match {
      case 'F' => F.pure(Front)
      case 'B' => F.pure(Back)
      case _   => F.raiseError(new IllegalArgumentException(s"'$char' is not a valid row"))
    }

  implicit val rowStrategy: Strategy[RowIndex] = {
    case RowIndex.Front => Strategy.Split.LowerHalf
    case RowIndex.Back  => Strategy.Split.UpperHalf
  }
}
