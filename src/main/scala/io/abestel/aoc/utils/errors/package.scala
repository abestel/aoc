package io.abestel.aoc.utils

import cats.ApplicativeError
import enumeratum._

import scala.reflect.ClassTag

package object errors {
  implicit class StringTransformerOps(private val string: String) extends AnyVal {
    def toIntF[F[_]](implicit F: ApplicativeError[F, Throwable]): F[Int] =
      F.fromOption(
        string.toIntOption,
        new IllegalArgumentException(s"'$string' is not a valid int"),
      )

    def toLongF[F[_]](implicit F: ApplicativeError[F, Throwable]): F[Long] =
      F.fromOption(
        string.toLongOption,
        new IllegalArgumentException(s"'$string' is not a valid long"),
      )

    def toEnum[F[_], EE <: EnumEntry: ClassTag](theEnum: Enum[EE])(implicit F: ApplicativeError[F, Throwable]): F[EE] =
      F.fromOption(
        theEnum.withNameOption(string),
        new IllegalArgumentException(s"'$string' is not a valid ${implicitly[ClassTag[EE]].runtimeClass.getSimpleName}"),
      )
  }
}
