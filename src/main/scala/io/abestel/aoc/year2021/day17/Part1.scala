package io.abestel.aoc.year2021.day17

object Part1 extends App {
  // target area: x=209..238, y=-86..-59
  val targetArea = TargetArea(209, 238, -86, -59)

  val highest = trajectories(targetArea).maxBy(_.map(_._2).max)
  println(prettyPrint(targetArea, highest))
  println(highest.map(_._2).max)
}
