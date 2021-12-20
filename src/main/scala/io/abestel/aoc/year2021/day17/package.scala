package io.abestel.aoc.year2021

package object day17 {
  def prettyPrint(
      targetArea: TargetArea,
      coordinates: List[(Int, Int)],
  ): String = {
    val minX = coordinates.map(_._1).min min targetArea.minX
    val maxX = coordinates.map(_._1).max max targetArea.maxX
    val minY = coordinates.map(_._2).min min targetArea.minY
    val maxY = coordinates.map(_._2).max max targetArea.maxY

    val coords = coordinates.toSet
    (maxY to minY by -1)
      .map { y =>
        (minX to maxX).map { x =>
          if (coords.contains((x, y))) {
            s"${Console.GREEN_B}#${Console.RESET}"
          } else if (targetArea.contains(x, y)) {
            s"${Console.BLUE}T${Console.RESET}"
          } else {
            s"${Console.YELLOW}.${Console.RESET}"
          }
        }.mkString
      }
      .mkString("\n")
  }

  def trajectories(targetArea: TargetArea): Iterator[List[(Int, Int)]] = {
    val TargetArea(_, maxX, minY, _) = targetArea

    def points(acceleration: Int, updateAcceleration: Int => Int): Iterator[Int] =
      Iterator
        .iterate((0, acceleration)) { case (coord, acceleration) =>
          (coord + acceleration, updateAcceleration(acceleration))
        }
        .map(_._1)

    def updateXAcceleration(acceleration: Int): Int =
      if (acceleration == 0) 0
      else if (acceleration < 0) acceleration + 1
      else acceleration - 1

    for {
      speedX <- Iterator.range(1, maxX + 1)
      speedY <- Iterator.range(minY, -minY + 1)
      xs          = points(speedX, updateXAcceleration).takeWhile(_ <= maxX)
      ys          = points(speedY, _ - 1).takeWhile(_ >= minY)
      coordinates = xs.zip(ys).toList
      if coordinates.exists { case (x, y) => targetArea.contains(x, y) }
    } yield coordinates
  }
}
