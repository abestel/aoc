package io.abestel.aoc.year2021.day8

import cats.ApplicativeError
import cats.implicits._
import io.abestel.aoc.utils.errors._

import scala.collection.immutable.SortedSet

case class Segments(
    segments: SortedSet[Segment]
) {
  val cardinality: Int = segments.size
}

object Segments {
  def parse[F[_]](rawSegments: String)(implicit F: ApplicativeError[F, Throwable]): F[Segments] =
    rawSegments.toList
      .traverse { segmentChar =>
        segmentChar.toString.toEnum[F, Segment](Segment)
      }
      .map(_.to(SortedSet))
      .map(Segments(_))
}
