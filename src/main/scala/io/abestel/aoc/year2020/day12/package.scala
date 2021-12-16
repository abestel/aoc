package io.abestel.aoc.year2020

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import io.abestel.aoc.utils.errors._

package object day12 {
  def data[F[_]: Files: Sync]: Stream[F, Command] =
    Fs2FilesExt
      .resourceStream[F]("2020/day12.txt")
      .evalMap[F, Command] {
        case s"N$unit" => unit.toLongF[F].map(Command.ToDirection(Direction.North, _))
        case s"S$unit" => unit.toLongF[F].map(Command.ToDirection(Direction.South, _))
        case s"E$unit" => unit.toLongF[F].map(Command.ToDirection(Direction.East, _))
        case s"W$unit" => unit.toLongF[F].map(Command.ToDirection(Direction.West, _))
        case s"L$unit" => unit.toLongF[F].map(Command.Rotate(Rotation.Left, _))
        case s"R$unit" => unit.toLongF[F].map(Command.Rotate(Rotation.Right, _))
        case s"F$unit" => unit.toLongF[F].map(Command.Forward.apply)
        case other     => Sync[F].raiseError(new IllegalArgumentException(s"'$other' is not a valid input"))
      }

}
