package io.abestel.aoc.utils.grid

import cats.data.State

trait Grid2DCompanion[T] {
  def makeIndexedMap(values: List[List[T]]): Map[Coordinates, T] =
    (for {
      (row, y)  <- values.zipWithIndex.iterator
      (cell, x) <- row.zipWithIndex
    } yield Coordinates(X(x), Y(y)) -> cell).toMap

  def visualize[G <: Grid2D[T]](): State[G, Unit] =
    State.inspect(grid => println(grid.visualize))
}
