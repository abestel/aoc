package io.abestel.aoc.year2021.day13

sealed trait CellState

object CellState {
  case object Marked         extends CellState
  case object Empty          extends CellState
  case object HorizontalFold extends CellState
  case object VerticalFold   extends CellState
}
