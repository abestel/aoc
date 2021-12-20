package io.abestel.aoc.year2021.day17

object Part2 extends App {
  // target area: x=209..238, y=-86..-59
  val targetArea = TargetArea(209, 238, -86, -59)

  println(trajectories(targetArea).size)
}
