package io.abestel.aoc.year2021.day13

import io.abestel.aoc.utils.grid._

case class Transparent(
    override val values: Map[Coordinates, CellState]
) extends Grid2D[CellState] {
  override def formatCell(coordinates: Coordinates, t: CellState): String =
    t match {
      case CellState.Marked         => s"${Console.GREEN_B}${Console.BOLD}#${Console.RESET}"
      case CellState.Empty          => formatEmptyCell
      case CellState.HorizontalFold => s"${Console.RED}${Console.BOLD}-${Console.RESET}"
      case CellState.VerticalFold   => s"${Console.RED}${Console.BOLD}|${Console.RESET}"
    }

  override val formatEmptyCell: String = s"${Console.YELLOW}.${Console.RESET}"

  def mark(x: X, y: Y): Transparent =
    copy(values.updated(Coordinates(x, y), CellState.Marked))

  def prepareHorizontalFold(y: Y): Transparent =
    copy(values ++ xRange.map(x => Coordinates(x, y) -> CellState.HorizontalFold).toMap)

  def processHorizontalFold(yFold: Y): Transparent =
    Transparent(
      values.iterator.flatMap {
        case (Coordinates(x, y), state) if y.value > yFold.value =>
          Some(
            Coordinates(x, Y(yFold.value + yFold.value - y.value)) -> state
          )

        case (_, CellState.HorizontalFold) =>
          None

        case other =>
          Some(other)
      }.toMap
    )

  def prepareVerticalFold(x: X): Transparent =
    copy(values ++ yRange.map(y => Coordinates(x, y) -> CellState.VerticalFold).toMap)

  def processVerticalFold(xFold: X): Transparent =
    Transparent(
      values.iterator.flatMap {
        case (Coordinates(x, y), state) if x.value > xFold.value =>
          Some(
            Coordinates(X(xFold.value + xFold.value - x.value), y) -> state
          )

        case (_, CellState.VerticalFold) =>
          None

        case other =>
          Some(other)
      }.toMap
    )

  def marked: List[Coordinates] =
    values.iterator.collect { case (coordinates, CellState.Marked) =>
      coordinates
    }.toList
}

object Transparent extends Grid2DCompanion[CellState] {
  val empty: Transparent = Transparent(Map.empty)
}
