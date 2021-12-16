package io.abestel.aoc.year2021.day5

import cats.effect.std.Console
import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      map <- dataStream[IO] {
        case (pos1, pos2) if pos1.x == pos2.x => range(pos1.y, pos2.y).map(Pos(pos1.x, _))
        case (pos1, pos2) if pos1.y == pos2.y => range(pos1.x, pos2.x).map(Pos(_, pos1.y))
        case (pos1, pos2) =>
          val xIterator =
            if (pos1.x < pos2.x) {
              pos1.x to pos2.x
            } else {
              (pos2.x to pos1.x).reverse
            }

          val yIterator =
            if (pos1.y < pos2.y) {
              pos1.y to pos2.y
            } else {
              (pos2.y to pos1.y).reverse
            }

          (xIterator zip yIterator).map { case (x, y) => Pos(x, y) }.toList
      }

      _ <- Console[IO].println(map.values.count(_ > 1))
    } yield ()

  def range(pos1: Int, pos2: Int): List[Int] =
    (if (pos1 < pos2) {
       pos1 to pos2
     } else {
       (pos2 to pos1).reverse
     }).toList
}
