package io.abestel.aoc.year2021.day8

import enumeratum._
import io.abestel.aoc.year2021.day8.Segment._

import scala.collection.immutable.SortedSet

sealed abstract class Number(val toInt: Int, val segments: SortedSet[Segment]) extends EnumEntry {
  val cardinality: Int = segments.size

  override def toString: String =
    s"${this.getClass.getSimpleName}"
}

/**
 *   0:      1:      2:      3:      4:
 *  aaaa    ....    aaaa    aaaa    ....
 * b    c  .    c  .    c  .    c  b    c
 * b    c  .    c  .    c  .    c  b    c
 *  ....    ....    dddd    dddd    dddd
 * e    f  .    f  e    .  .    f  .    f
 * e    f  .    f  e    .  .    f  .    f
 *  gggg    ....    gggg    gggg    ....
 *
 *   5:      6:      7:      8:      9:
 *  aaaa    aaaa    aaaa    aaaa    aaaa
 * b    .  b    .  .    c  b    c  b    c
 * b    .  b    .  .    c  b    c  b    c
 *  dddd    dddd    ....    dddd    dddd
 * .    f  e    f  .    f  e    f  .    f
 * .    f  e    f  .    f  e    f  .    f
 *  gggg    gggg    ....    gggg    gggg
 */
object Number extends Enum[Number] {
  override def values: IndexedSeq[Number] = super.findValues

  case object Zero  extends Number(0, SortedSet(A, B, C, E, F, G))
  case object One   extends Number(1, SortedSet(C, F))
  case object Two   extends Number(2, SortedSet(A, C, D, E, G))
  case object Three extends Number(3, SortedSet(A, C, D, F, G))
  case object Four  extends Number(4, SortedSet(B, C, D, F))
  case object Five  extends Number(5, SortedSet(A, B, D, F, G))
  case object Six   extends Number(6, SortedSet(A, B, D, E, F, G))
  case object Seven extends Number(7, SortedSet(A, C, F))
  case object Eight extends Number(8, SortedSet(A, B, C, D, E, F, G))
  case object Nine  extends Number(9, SortedSet(A, B, C, D, F, G))

  def unapply(number: Number): Some[SortedSet[Segment]] =
    Some(number.segments)

  val cardinalityToNumbers: Map[Int, Set[Number]] =
    values.groupMapReduce(_.cardinality)(Set(_))(_ union _)

  val segmentsToNumbers: Map[SortedSet[Segment], Set[Number]] =
    values.groupMapReduce(_.segments)(Set(_))(_ union _)

  implicit val numberOrdering: Ordering[Number] =
    Ordering.by[Number, Int](_.toInt)
}
