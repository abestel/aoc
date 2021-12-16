package io.abestel.aoc.year2021.day6

case class FishWorld(
    ageToPopulation: Map[Int, Long]
) {
  def nextDay: FishWorld =
    FishWorld {
      ageToPopulation.toList
        .flatMap {
          case (age, count) if age == 0 => List(8 -> count, 6 -> count)
          case (age, count)             => List(age - 1 -> count)
        }
        .groupMapReduce(_._1)(_._2)(_ + _)
    }
}

object FishWorld {
  def fromAgeList(ages: List[Int]): FishWorld =
    FishWorld(
      ages.groupMapReduce(identity)(_ => 1L)(_ + _)
    )
}
