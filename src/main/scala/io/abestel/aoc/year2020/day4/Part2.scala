package io.abestel.aoc.year2020.day4

import cats.data.ReaderT
import cats.effect.std.Console
import cats.effect.{IO, IOApp, Sync}
import cats.implicits._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.OneOf
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.string.MatchesRegex
import io.abestel.aoc.utils.refined.RefinedHelper
import io.abestel.aoc.utils.shapeless._
import shapeless._

object Part2 extends IOApp.Simple {
  type BirthYear      = Int Refined Interval.Closed[1920, 2002]
  type IssueYear      = Int Refined Interval.Closed[2010, 2020]
  type ExpirationYear = Int Refined Interval.Closed[2020, 2030]
  type HairColor      = String Refined MatchesRegex["#[0-9a-f]{6}"]
  type EyeColor   = String Refined OneOf[Equal["amb"] :: Equal["blu"] :: Equal["brn"] :: Equal["gry"] :: Equal["grn"] :: Equal["hzl"] :: Equal["oth"] :: HNil]
  type PassportId = String Refined MatchesRegex["[0-9]{9}"]

  sealed trait Height

  object Height extends RefinedHelper {
    type HeightCentimeters = Interval.Closed[150, 193]
    type HeightInches      = Interval.Closed[59, 76]

    final case class Centimeters(height: Int Refined HeightCentimeters) extends Height
    final case class Inches(height: Int Refined HeightInches)           extends Height

    private val heightExtractor = "(?<size>[0-9]+)(?<unit>cm|in)".r
    implicit def heightReaderT[F[_]: Sync]: ReaderT[F, String, Height] =
      ReaderT {
        case heightExtractor(rawSize, unit) =>
          for {
            size <- Sync[F].fromOption(
              rawSize.toIntOption,
              new IllegalArgumentException(s"'$rawSize' is not an integer"),
            )

            height <- unit match {
              case "cm" => refineF[F, Int, HeightCentimeters](size).map(Centimeters)
              case "in" => refineF[F, Int, HeightInches](size).map(Inches)
              case _    => Sync[F].raiseError(new IllegalArgumentException(s"'$unit' is not a valid unit"))
            }
          } yield height

        case other =>
          Sync[F].raiseError(
            new IllegalArgumentException(s"'$other' is not a valid height")
          )
      }
  }

  final case class Passport(
      byr: BirthYear,
      iyr: IssueYear,
      eyr: ExpirationYear,
      hgt: Height,
      hcl: HairColor,
      ecl: EyeColor,
      pid: PassportId,
  )

  object Passport {
    def tryReadPassport[F[_]: Sync](
        keyValues: Map[String, String]
    ): F[Throwable Either Passport] =
      implicitly[ReaderT[F, Map[String, String], Passport]].run(keyValues).attempt
  }

  override def run: IO[Unit] =
    dataStream[IO]
      .evalMap(Passport.tryReadPassport[IO](_))
      .filter(_.isRight)
      .compile
      .count
      .flatMap(Console[IO].println)
}
