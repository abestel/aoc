package io.abestel.aoc.year2020.day4

import cats.data.ReaderT
import cats.effect.std.Console
import cats.effect.{IO, IOApp, Sync}
import cats.implicits._
import io.abestel.aoc.utils.shapeless._

object Part1 extends IOApp.Simple {

  final case class Passport(
      byr: String,
      iyr: String,
      eyr: String,
      hgt: String,
      hcl: String,
      ecl: String,
      pid: String,
      cid: Option[String],
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
