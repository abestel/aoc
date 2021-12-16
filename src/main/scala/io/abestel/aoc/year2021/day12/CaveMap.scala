package io.abestel.aoc.year2021.day12

import cats.implicits._

case class CaveMap(
    connections: Map[Cave, Set[Cave]] = Map.empty
) {
  def addConnection(cave1: Cave, cave2: Cave): CaveMap =
    CaveMap {
      connections
        .updatedWith(cave1)(_.fold(Set(cave2))(_ + cave2).some)
        .updatedWith(cave2)(_.fold(Set(cave1))(_ + cave1).some)
    }
}
