package io.abestel.aoc.year2021.day12

import cats.Show

sealed trait Cave {
  def isBig: Boolean =
    this match {
      case Cave.Big(_) => true
      case _           => false
    }
}

object Cave {
  case class Small(name: String) extends Cave
  case class Big(name: String)   extends Cave
  case object Start              extends Cave
  case object End                extends Cave

  def parse(name: String): Cave =
    name match {
      case "start"                      => Start
      case "end"                        => End
      case big if big.forall(_.isUpper) => Big(big)
      case small                        => Small(small)
    }

  implicit val show: Show[Cave] = Show.show {
    case Small(name) => name
    case Big(name)   => name
    case Start       => "Start"
    case End         => "End"
  }
}
