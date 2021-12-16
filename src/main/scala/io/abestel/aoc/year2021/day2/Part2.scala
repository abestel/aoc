package io.abestel.aoc.year2021.day2

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  final case class Submarine(
      x: Long = 0L,
      y: Long = 0L,
      aim: Long = 0L,
  )

  implicit val commandable: Commandable[Submarine] =
    Commandable.instance(Submarine()) {
      case Command.Forward(delta) => t => t.copy(x = t.x + delta, y = t.y + delta * t.aim)
      case Command.Up(delta)      => t => t.copy(aim = t.aim - delta)
      case Command.Down(delta)    => t => t.copy(aim = t.aim + delta)
    }

  override def run: IO[Unit] =
    stream[IO, Submarine]
      .map { case Submarine(x, y, _) => x * y }
      .flatMap(Console[IO].println)
}
