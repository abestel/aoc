package io.abestel.aoc.year2021.day8

import enumeratum._
import enumeratum.EnumEntry.Lowercase

sealed trait Segment extends EnumEntry with Lowercase

object Segment extends Enum[Segment] {
  override def values: IndexedSeq[Segment] = super.findValues

  case object A extends Segment
  case object B extends Segment
  case object C extends Segment
  case object D extends Segment
  case object E extends Segment
  case object F extends Segment
  case object G extends Segment

  implicit val segmentOrdering: Ordering[Segment] =
    Ordering.by[Segment, Char] {
      case A => 'A'
      case B => 'B'
      case C => 'C'
      case D => 'D'
      case E => 'E'
      case F => 'F'
      case G => 'G'
    }
}
