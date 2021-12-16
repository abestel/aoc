package io.abestel.aoc.utils.shapeless

import cats.{Applicative, ApplicativeError, FlatMap}
import cats.data.ReaderT
import cats.implicits._

trait ReaderTBaseDerivationInstances {
  implicit def fromSomeValue[F[_], T](implicit
      flatMap: FlatMap[F],
      applicativeError: ApplicativeError[F, Throwable],
      tReader: ReaderT[F, String, T],
  ): ReaderT[F, Option[String], T] =
    ReaderT[F, Option[String], String] {
      ApplicativeError[F, Throwable].fromOption(
        _,
        new IllegalArgumentException("Trying to decode empty value"),
      )
    }.andThen(tReader)

  implicit def fromOptionValue[F[_]: Applicative, T](implicit
      tReader: ReaderT[F, String, T]
  ): ReaderT[F, Option[String], Option[T]] =
    ReaderT(_.traverse(tReader.run))

  implicit def readInt[F[_]](implicit F: ApplicativeError[F, Throwable]): ReaderT[F, String, Int] =
    ReaderT(from =>
      F.fromOption(
        from.toIntOption,
        new IllegalArgumentException(s"$from is not a valid integer"),
      )
    )

  implicit def readString[F[_]: Applicative]: ReaderT[F, String, String] =
    ReaderT(Applicative[F].pure(_))
}
