package io.abestel.aoc.year2020.day8

import cats.ApplicativeError

sealed trait Command

object Command {
  case class NoOp(delta: Int)       extends Command
  case class Accumulate(delta: Int) extends Command
  case class Jump(delta: Int)       extends Command

  def parse[F[_]](rawCommand: String)(implicit F: ApplicativeError[F, Throwable]): F[Command] =
    rawCommand
      .split(" ") match {
      case Array("nop", delta) => F.pure(NoOp(delta.toInt))
      case Array("acc", delta) => F.pure(Accumulate(delta.toInt))
      case Array("jmp", delta) => F.pure(Jump(delta.toInt))
      case other               => F.raiseError(new IllegalArgumentException(s"'${other.mkString(" ")}' is not a valid command"))
    }
}
