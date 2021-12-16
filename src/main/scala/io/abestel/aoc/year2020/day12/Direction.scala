package io.abestel.aoc.year2020.day12

import enumeratum._

sealed abstract class Direction(val angle: Long) extends EnumEntry

object Direction extends Enum[Direction] {
  override def values: IndexedSeq[Direction] = super.findValues

  case object North extends Direction(270L)
  case object South extends Direction(90L)
  case object East  extends Direction(0L)
  case object West  extends Direction(180L)

  val angleToDirection: Map[Long, Direction] =
    values.map(direction => direction.angle -> direction).toMap
}
