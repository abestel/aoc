package io.abestel.aoc.year2021.day18

import cats.Show
import cats.Semigroup
import cats.parse.{Numbers, Parser}
import cats.syntax.show._

import scala.annotation.tailrec

sealed trait Snailfish {
  def addLeft(delta: Long): Snailfish =
    this match {
      case Snailfish.RegularNumber(value) => Snailfish.RegularNumber(value + delta)
      case Snailfish.Pair(left, right)    => Snailfish.Pair(left addLeft delta, right)
    }

  def addRight(delta: Long): Snailfish =
    this match {
      case Snailfish.RegularNumber(value) => Snailfish.RegularNumber(value + delta)
      case Snailfish.Pair(left, right)    => Snailfish.Pair(left, right addRight delta)
    }

  def magnitude: Long =
    this match {
      case Snailfish.RegularNumber(value) => value
      case Snailfish.Pair(left, right)    => 3 * left.magnitude + 2 * right.magnitude
    }
}

object Snailfish {
  case class RegularNumber(value: Long)              extends Snailfish
  case class Pair(left: Snailfish, right: Snailfish) extends Snailfish

  val parser: Parser[Snailfish] = Parser.recursive[Snailfish] { recurse =>
    val number = Numbers.digits.map(_.toLong)

    val regularNumber: Parser[RegularNumber] =
      number.map(RegularNumber)

    val pair =
      (recurse ~ (Parser.char(',') *> recurse))
        .between(Parser.char('['), Parser.char(']'))
        .map { case (left, right) => Snailfish.Pair(left, right) }

    Parser.oneOf(regularNumber :: pair :: Nil)
  }

  implicit lazy val show: Show[Snailfish] = Show.show {
    case RegularNumber(value) => value.toString
    case Pair(left, right)    => s"[${left.show},${right.show}]"
  }

  case class Explosion(maybeAddLeft: Option[Long], newNode: Snailfish, maybeAddRight: Option[Long]) {
    def appendRight(right: Snailfish): Explosion =
      copy(
        newNode = Pair(newNode, maybeAddRight.fold(right)(right.addLeft)),
        maybeAddRight = None,
      )

    def appendLeft(left: Snailfish): Explosion =
      copy(
        maybeAddLeft = None,
        newNode = Pair(maybeAddLeft.fold(left)(left.addRight), newNode),
      )
  }

  object Explosion {
    def apply(left: RegularNumber, right: RegularNumber): Explosion =
      Explosion(Some(left.value), RegularNumber(0), Some(right.value))
  }

  implicit val semigroup: Semigroup[Snailfish] = Semigroup.instance { case (left, right) =>
    def explode(snailfish: Snailfish): Option[Snailfish] = {
      def go(current: Snailfish = snailfish, depth: Int = 0): Option[Explosion] = current match {
        case RegularNumber(_)                                              => None
        case Pair(left: RegularNumber, right: RegularNumber) if depth >= 4 => Some(Explosion(left, right))
        case Pair(left, right) =>
          go(left, depth + 1)
            .map(_.appendRight(right))
            .orElse(
              go(right, depth + 1)
                .map(_.appendLeft(left))
            )
      }

      go().map(_.newNode)
    }

    def split(snailfish: Snailfish): Option[Snailfish] = snailfish match {
      case RegularNumber(value) if value > 9 =>
        val half = value.toFloat / 2
        Some(Pair(RegularNumber(half.floor.toLong), RegularNumber(half.ceil.toLong)))
      case RegularNumber(_)  => None
      case Pair(left, right) => split(left).map(Pair(_, right)) orElse split(right).map(Pair(left, _))
    }

    @tailrec
    def reduce(snailfish: Snailfish): Snailfish =
      explode(snailfish).orElse(split(snailfish)) match {
        case Some(value) => reduce(value)
        case None        => snailfish
      }

    reduce(Pair(left, right))
  }
}
