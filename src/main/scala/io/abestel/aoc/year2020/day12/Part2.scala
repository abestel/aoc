package io.abestel.aoc.year2020.day12

import cats.{Applicative, Functor}
import cats.effect.{IO, IOApp, Sync}
import cats.implicits._

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO]
      .evalScan(Boat.empty) { case (boat, command) =>
        command match {
          case Command.ToDirection(direction, unit) => boat.modifyWayPoint(_.move[IO](direction, unit))
          case Command.Forward(unit)                => IO.pure(boat.move(unit))
          case Command.Rotate(rotation, unit)       => boat.modifyWayPoint(_.rotate[IO](rotation, unit))
        }
      }
      .debug()
      .compile
      .lastOrError
      .map { case Boat(_, posX, posY) =>
        posX.abs + posY.abs
      }
      .map(println)

  case class Boat(
      wayPoint: WayPoint,
      posX: Long,
      posY: Long,
  ) {
    def modifyWayPoint[F[_]: Functor](fn: WayPoint => F[WayPoint]): F[Boat] =
      fn(wayPoint).map(newWayPoint => copy(wayPoint = newWayPoint))

    def move(unit: Long): Boat = {
      def moveInDir(posX: Long, posY: Long, wayPointDirection: Direction, wayPointUnit: Long): (Long, Long) =
        wayPointDirection match {
          case Direction.North => (posX, posY - wayPointUnit * unit)
          case Direction.South => (posX, posY + wayPointUnit * unit)
          case Direction.East  => (posX + wayPointUnit * unit, posY)
          case Direction.West  => (posX - wayPointUnit * unit, posY)
        }

      val (posX1, posY1) = moveInDir(posX, posY, wayPoint.dir1, wayPoint.unit1)
      val (posX2, posY2) = moveInDir(posX1, posY1, wayPoint.dir2, wayPoint.unit2)
      copy(posX = posX2, posY = posY2)
    }
  }

  object Boat {
    val empty: Boat = Boat(wayPoint = WayPoint.empty, posX = 0L, posY = 0L)
  }

  case class WayPoint(
      dir1: Direction,
      unit1: Long,
      dir2: Direction,
      unit2: Long,
  ) {
    import Direction._

    def move[F[_]: Applicative](direction: Direction, unit: Long): F[WayPoint] = {
      def moveWaypoint(wayPointDirection: Direction, wayPointUnit: Long): Long =
        (direction, wayPointDirection) match {
          case (North, North) | (South, South) | (East, East) | (West, West) => wayPointUnit + unit
          case (North, South) | (South, North) | (West, East) | (East, West) => wayPointUnit - unit
          case _                                                             => wayPointUnit
        }

      val newUnit1 = moveWaypoint(dir1, unit1)
      val newUnit2 = moveWaypoint(dir2, unit2)
      Applicative[F].pure(copy(unit1 = newUnit1, unit2 = newUnit2))
    }

    def rotate[F[_]: Sync](rotation: Rotation, unit: Long): F[WayPoint] = {
      def rotateAngle(direction: Direction): F[Direction] = {
        val newAngle          = direction.angle + (unit * rotation.ratio)
        val newAngleSanitized = (Math.floorMod(newAngle, 360L) + 360) % 360
        Sync[F].fromOption(
          Direction.angleToDirection.get(newAngleSanitized),
          new IllegalArgumentException(s"Can't rotate direction $direction $unit degrees to the $rotation because that does not match any known direction"),
        )
      }

      (
        rotateAngle(dir1),
        rotateAngle(dir2),
      ).tupled.map { case (newDir1, newDir2) =>
        copy(dir1 = newDir1, dir2 = newDir2)
      }
    }
  }

  object WayPoint {
    val empty: WayPoint = WayPoint(Direction.East, 10L, Direction.North, 1)
  }
}
