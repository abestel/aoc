package io.abestel.aoc.year2020.day7

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._

sealed trait BagRule

object BagRule {
  case object Empty                                              extends BagRule
  case class Contains(description: NonEmptyList[(BagName, Int)]) extends BagRule

  def parse[F[_]: Sync](rawRule: String): F[(BagName, BagRule)] =
    rawRule.split(" bags contain ") match {
      case Array(bag, "no other bags.") =>
        Sync[F].pure(bag -> Empty)

      case Array(bag, description) =>
        for {
          contains <- description
            .split(",|\\.")
            .map(_.trim)
            .collect { case string if string.nonEmpty => string.split(" ") }
            .toList
            .traverse[F, (String, Int)] {
              case Array(number, adjective, color, _) =>
                Sync[F]
                  .fromOption(
                    number.toIntOption,
                    new IllegalArgumentException(s"'$number' is not an integer"),
                  )
                  .map(s"$adjective $color" -> _)

              case other =>
                Sync[F]
                  .raiseError(
                    new IllegalArgumentException(
                      s"'${other.mkString(" ")}' is not a correct subrule"
                    )
                  )
            }

          containsNel <- Sync[F].fromOption(
            contains.toNel,
            new IllegalArgumentException(s"Bag '$bag' does not have a valid set of rules"),
          )
        } yield bag -> Contains(containsNel)

      case _ =>
        Sync[F].raiseError(
          new IllegalArgumentException(s"Rule '$rawRule' is no valid")
        )
    }
}
