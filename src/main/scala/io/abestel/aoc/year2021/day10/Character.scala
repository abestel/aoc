package io.abestel.aoc.year2021.day10

import enumeratum._

sealed trait Character                                   extends EnumEntry
sealed abstract class Closer                             extends Character
sealed abstract class Opener[C <: Closer](val closer: C) extends Character

object Character extends Enum[Character] {
  override def values: IndexedSeq[Character] = super.findValues

  case object `(` extends Opener(`)`)
  case object `)` extends Closer
  case object `{` extends Opener(`}`)
  case object `}` extends Closer
  case object `[` extends Opener(`]`)
  case object `]` extends Closer
  case object `<` extends Opener(`>`)
  case object `>` extends Closer

  val openers: List[Opener[_]] =
    values.collect { case o: Opener[_] => o }.toList
}
