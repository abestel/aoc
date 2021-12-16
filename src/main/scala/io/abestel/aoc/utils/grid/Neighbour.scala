package io.abestel.aoc.utils.grid

sealed trait Neighbour {
  def deltaX: Int
  def deltaY: Int
}

object Neighbour {
  def unapply(neighbour: Neighbour): Some[(Int, Int)] =
    Some(
      (
        neighbour.deltaX,
        neighbour.deltaY,
      )
    )

  sealed abstract class Base(val deltaX: Int, val deltaY: Int) extends Neighbour

  case object Up    extends Base(0, -1)
  case object Down  extends Base(0, 1)
  case object Left  extends Base(-1, 0)
  case object Right extends Base(1, 0)

  sealed abstract class Composite(elements: List[Base]) extends Neighbour {
    override val deltaX: Int = elements.map(_.deltaX).sum
    override val deltaY: Int = elements.map(_.deltaY).sum
  }

  case object UpLeft    extends Composite(List(Up, Left))
  case object UpRight   extends Composite(List(Up, Right))
  case object DownLeft  extends Composite(List(Down, Left))
  case object DownRight extends Composite(List(Down, Right))

  val base: List[Neighbour]      = List(Up, Down, Left, Right)
  val diagonals: List[Neighbour] = List(UpLeft, UpRight, DownLeft, DownRight)
  val all: List[Neighbour]       = base ++ diagonals
}
