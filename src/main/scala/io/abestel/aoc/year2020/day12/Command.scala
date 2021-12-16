package io.abestel.aoc.year2020.day12

sealed trait Command

object Command {
  case class ToDirection(direction: Direction, unit: Long) extends Command
  case class Forward(unit: Long)                           extends Command
  case class Rotate(rotation: Rotation, unit: Long)        extends Command
}
