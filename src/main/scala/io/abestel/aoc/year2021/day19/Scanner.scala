package io.abestel.aoc.year2021.day19

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._
import io.abestel.aoc.utils.geom.{IntMatrix3, Pos3}

import scala.annotation.tailrec

case class Scanner(position: Pos3, probes: NonEmptySet[Pos3])

object Scanner {
  def align(probes: NonEmptyList[NonEmptySet[Pos3]]): List[Scanner] = {
    @tailrec
    def loop(
        scanners: List[Scanner],
        allProbes: NonEmptySet[Pos3],
        remaining: List[NonEmptySet[Pos3]],
    ): List[Scanner] =
      remaining match {
        case Nil => scanners
        case _ =>
          val (probes, rotation, translation, _) = remaining.iterator
            .map { probes =>
              val (rotation, translation, count) = IntMatrix3.rotation90degreesAllAngles.iterator
                .map { rotation =>
                  val oriented = probes.map(_.transform(rotation))

                  val (translation, count) = (for {
                    pos1 <- allProbes.toSortedSet.iterator
                    pos2 <- oriented.toSortedSet.iterator
                  } yield pos1 - pos2).toList.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)

                  (rotation, translation, count)
                }
                .maxBy(_._3)

              (probes, rotation, translation, count)
            }
            .maxBy(_._4)

          val alignedProbes = probes.map(_.transform(rotation)).map(_ + translation)
          loop(
            scanners = scanners :+ Scanner(translation, alignedProbes),
            allProbes = allProbes union alignedProbes,
            remaining = remaining.filterNot(_ == probes),
          )
      }

    loop(
      List(Scanner(Pos3(0, 0, 0), probes.head)),
      probes.head,
      probes.tail,
    )
  }
}
