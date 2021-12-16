package io.abestel.aoc.year2021.day2

abstract class Commandable[T] {
  def zero: T
  def accept(t: T, command: Command): T
}

object Commandable {
  def apply[T](implicit commandable: Commandable[T]): Commandable[T] = commandable

  def instance[T](zeroFn: T)(logic: Command => T => T): Commandable[T] =
    new Commandable[T] {
      override def zero: T                           = zeroFn
      override def accept(t: T, command: Command): T = logic(command)(t)
    }
}
