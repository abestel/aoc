package io.abestel.aoc.utils.shapeless

import cats.{ApplicativeError, FlatMap}
import cats.data.ReaderT
import eu.timepit.refined.api.{Refined, Validate}
import io.abestel.aoc.utils.refined.RefinedHelper

trait ReaderTRefinedDerivationInstances extends RefinedHelper {
  implicit def refinedStringReader[F[_], P](implicit
      F: ApplicativeError[F, Throwable],
      validate: Validate[String, P],
  ): ReaderT[F, String, String Refined P] =
    ReaderT(refineF[F, String, P](_))

  implicit def refinedReader[F[_]: FlatMap, T, P](implicit
      F: ApplicativeError[F, Throwable],
      trf: ReaderT[F, String, T],
      validate: Validate[T, P],
  ): ReaderT[F, String, T Refined P] =
    trf.flatMapF(from => refineF[F, T, P](from))
}
