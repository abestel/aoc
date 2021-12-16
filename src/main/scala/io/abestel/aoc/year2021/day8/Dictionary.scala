package io.abestel.aoc.year2021.day8

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

case class Dictionary(
    inputToOutput: Map[Segment, SortedSet[Segment]]
) {
  def restrict(inputSegments: SortedSet[Segment], outputSegments: SortedSet[Segment]): Dictionary =
    Dictionary {
      inputToOutput.view.map {
        case (in, out) if inputSegments contains in => in -> (out intersect outputSegments)
        case (in, out)                              => in -> (out diff outputSegments)
      }.toMap
    }

  def decode(input: Segments): List[Number] = {
    @tailrec
    def combinations(
        remaining: List[Segment],
        result: Set[SortedSet[Segment]] = Set.empty,
    ): Set[SortedSet[Segment]] = {
      remaining match {
        case Nil =>
          result.filter(_.size == input.cardinality)

        case head :: tail =>
          val newResult = if (result.isEmpty) {
            inputToOutput(head).iterator.map(SortedSet(_)).toSet
          } else {
            result.flatMap { decodedSoFar =>
              inputToOutput(head).iterator.map { outputSegment =>
                decodedSoFar ++ Set(outputSegment)
              }
            }
          }

          combinations(
            remaining = tail,
            result = newResult,
          )
      }
    }

    for {
      combination <- combinations(input.segments.toList).toList
      number      <- Number.segmentsToNumbers.getOrElse(combination, Set.empty)
    } yield number
  }
}

object Dictionary {
  def empty: Dictionary =
    Dictionary {
      Segment.values.iterator.map(_ -> Segment.values.to(SortedSet)).toMap
    }
}
