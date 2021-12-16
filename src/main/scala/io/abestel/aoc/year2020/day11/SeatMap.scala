package io.abestel.aoc.year2020.day11

import cats.data.State
import cats.implicits._
import io.abestel.aoc.utils.grid._

case class SeatMap(
    override val values: Map[Coordinates, Element]
) extends Grid2D[Element] {
  override def formatCell(coordinates: Coordinates, t: Element): String = t match {
    case Element.Ground       => s"${Console.YELLOW}${Element.Ground.entryName}${Console.RESET}"
    case Element.FreeSeat     => s"${Console.GREEN}${Element.FreeSeat.entryName}${Console.RESET}"
    case Element.OccupiedSeat => s"${Console.RED}${Element.OccupiedSeat.entryName}${Console.RESET}"
  }
}

object SeatMap extends Grid2DCompanion[Element] {
  def apply(seats: List[List[Element]]): SeatMap =
    SeatMap {
      makeIndexedMap(seats)
    }

  def step(
      occupiedNeighboursToFree: Int,
      neighbours: (SeatMap, Coordinates) => List[(Coordinates, Element)],
  ): State[SeatMap, Int] =
    State[SeatMap, Int] { seatMap =>
      val changes = seatMap.values.iterator.collect {
        case (coordinates, Element.FreeSeat) if neighbours(seatMap, coordinates).forall { case (_, element) => element.isFree || element.isGround } =>
          (coordinates, Element.OccupiedSeat)

        case (coordinates, Element.OccupiedSeat) if neighbours(seatMap, coordinates).count { case (_, element) =>
              element.isOccupied
            } >= occupiedNeighboursToFree =>
          (coordinates, Element.FreeSeat)
      }.toMap

      val newSeatMap = SeatMap(seatMap.values ++ changes)

      (newSeatMap -> changes.size)
    }.flatTap(_ => visualize())
}
