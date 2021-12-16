package io.abestel.aoc.year2021

import cats.data.NonEmptyList
import cats.effect.Sync
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

import scala.annotation.tailrec

package object day12 {

  def data[F[_]: Files: Sync]: F[CaveMap] =
    Fs2FilesExt
      .resourceStream[F]("2021/day12.txt")
      .evalMap(
        _.split("-") match {
          case Array(cave1, cave2) => Sync[F].pure((Cave.parse(cave1), Cave.parse(cave2)))
          case other               => Sync[F].raiseError[(Cave, Cave)](new IllegalArgumentException(s"Invalid input '${other.mkString("-")}'"))
        }
      )
      .fold(CaveMap()) { case (caveMap, (cave1, cave2)) =>
        caveMap.addConnection(cave1, cave2)
      }
      .compile
      .lastOrError

  def paths(
      expand: (NonEmptyList[Cave], List[Cave]) => List[NonEmptyList[Cave]]
  )(
      caveMap: CaveMap
  ): List[NonEmptyList[Cave]] = {
    @tailrec
    def go(
        inProgress: List[NonEmptyList[Cave]],
        result: List[NonEmptyList[Cave]] = List.empty,
    ): List[NonEmptyList[Cave]] =
      inProgress match {
        case Nil => result.map(_.reverse).distinct
        case nonEmpty =>
          val (newResult, newInProgress) = nonEmpty
            .flatMap { case nel @ NonEmptyList(head, _) =>
              expand(
                nel,
                caveMap.connections
                  .getOrElse(head, Set.empty)
                  .toList,
              )
            }
            .partition {
              case NonEmptyList(Cave.End, _) => true
              case _                         => false
            }

          go(
            inProgress = newInProgress,
            result = result ++ newResult,
          )
      }

    go(
      List(
        NonEmptyList.one(Cave.Start)
      )
    )
  }
}
