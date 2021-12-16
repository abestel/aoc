package io.abestel.aoc.year2021.day2

import cats.ApplicativeError
import cats.effect.Sync

sealed trait Command extends Product with Serializable

object Command {
  final case class Forward(delta: Long) extends Command
  final case class Up(delta: Long)      extends Command
  final case class Down(delta: Long)    extends Command

  def parse[F[_]: Sync](raw: String): F[Command] = ApplicativeError[F, Throwable].fromOption(
    raw.split(" ") match {
      case Array(rawCommand, rawDelta) =>
        for {
          delta <- rawDelta.toLongOption
          command <- rawCommand match {
            case "forward" => Some(Forward(delta))
            case "up"      => Some(Up(delta))
            case "down"    => Some(Down(delta))
            case _         => None
          }
        } yield command

      case _ => None
    },
    new IllegalArgumentException(s"Command '$raw' is not valid"),
  )
}
