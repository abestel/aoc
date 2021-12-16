package io.abestel.aoc.utils

import io.estatico.newtype.macros.newtype

package object grid {
  @newtype case class X(value: Int)
  @newtype case class Y(value: Int)
}
