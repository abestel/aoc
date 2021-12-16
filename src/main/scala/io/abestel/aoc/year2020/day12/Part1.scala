package io.abestel.aoc.year2020.day12

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .evalScan(Boat.empty) { case (boat, command) =>
        command match {
          case Command.ToDirection(direction, unit) => IO.pure(boat.move(direction, unit))
          case Command.Forward(unit)                => IO.pure(boat.move(boat.direction, unit))
          case Command.Rotate(rotation, unit) =>
            val newAngle          = boat.direction.angle + (unit * rotation.ratio)
            val newAngleSanitized = (Math.floorMod(newAngle, 360L) + 360) % 360
            IO
              .fromOption(
                Direction.angleToDirection.get(newAngleSanitized)
              )(
                new IllegalArgumentException(s"Can't rotate boat $boat $unit degrees to the $rotation because that does not match any known direction")
              )
              .map(boat.withDirection)
        }
      }
      .debug()
      .compile
      .lastOrError
      .map { case Boat(_, posX, posY) =>
        posX.abs + posY.abs
      }
      .map(println)

  case class Boat(direction: Direction, posX: Long, posY: Long) {
    def move(direction: Direction, unit: Long): Boat =
      direction match {
        case Direction.North => copy(posY = posY - unit)
        case Direction.South => copy(posY = posY + unit)
        case Direction.East  => copy(posX = posX + unit)
        case Direction.West  => copy(posX = posX - unit)
      }

    def withDirection(newDirection: Direction): Boat =
      copy(direction = newDirection)
  }

  object Boat {
    val empty: Boat = Boat(direction = Direction.East, posX = 0L, posY = 0L)
  }
}
