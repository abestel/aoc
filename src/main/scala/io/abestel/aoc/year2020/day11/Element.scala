package io.abestel.aoc.year2020.day11

import enumeratum._

sealed abstract class Element(override val entryName: String) extends EnumEntry {
  def isGround: Boolean =
    this match {
      case Element.Ground => true
      case _              => false
    }

  def isFree: Boolean =
    this match {
      case Element.FreeSeat => true
      case _                => false
    }

  def isOccupied: Boolean =
    this match {
      case Element.OccupiedSeat => true
      case _                    => false
    }
}

object Element extends Enum[Element] {
  override def values: IndexedSeq[Element] = super.findValues

  case object Ground       extends Element(".")
  case object FreeSeat     extends Element("L")
  case object OccupiedSeat extends Element("#")
}
