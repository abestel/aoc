package io.abestel.aoc.year2021.day5

import cats.effect.{IO, IOApp}
import cats.effect.std.Console

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      map <- dataStream[IO] {
        case (pos1, pos2) if pos1.x == pos2.x =>
          (if (pos1.y < pos2.y) {
             pos1.y to pos2.y
           } else {
             pos2.y to pos1.y
           }).map(Pos(pos1.x, _)).toList

        case (pos1, pos2) if pos1.y == pos2.y =>
          (if (pos1.x < pos2.x) {
             pos1.x to pos2.x
           } else {
             pos2.x to pos1.x
           }).map(Pos(_, pos1.y)).toList

        case _ => List.empty
      }

      _ <- Console[IO].println(map.values.count(_ > 1))
    } yield ()
}
