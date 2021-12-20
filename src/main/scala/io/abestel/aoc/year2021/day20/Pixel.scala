package io.abestel.aoc.year2021.day20

sealed trait Pixel

object Pixel {
  case object Dark  extends Pixel
  case object Light extends Pixel
}
