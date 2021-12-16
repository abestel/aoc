package io.abestel.aoc.year2020.day12

import enumeratum._

sealed abstract class Rotation(val ratio: Int) extends EnumEntry

object Rotation extends Enum[Rotation] {
  override def values: IndexedSeq[Rotation] = super.findValues

  case object Left  extends Rotation(-1)
  case object Right extends Rotation(1)
}
