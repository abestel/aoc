package io.abestel.aoc.utils.refined

import cats.ApplicativeError
import cats.implicits._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.refineV

trait RefinedHelper {
  def refineF[F[_], T, P](t: T)(implicit
      F: ApplicativeError[F, Throwable],
      validate: Validate[T, P],
  ): F[T Refined P] =
    F.fromEither(
      refineV[P](t)
        .leftMap(new IllegalArgumentException(_))
    )
}
