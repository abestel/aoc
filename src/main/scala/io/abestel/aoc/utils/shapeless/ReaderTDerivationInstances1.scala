package io.abestel.aoc.utils.shapeless

import cats.{Applicative, Functor, Semigroupal}
import cats.data.ReaderT
import cats.implicits._
import shapeless.{::, Generic, HList, HNil, Lazy}

trait ReaderTDerivationInstances1 {
  implicit def hnilDecoder[F[_]: Applicative]: ReaderT[F, Map[String, String], HNil] =
    ReaderT.pure(HNil)

  implicit def productDecoder[F[_]: Functor, P, T](implicit
      gen: Generic.Aux[P, T],
      tReader: ReaderT[F, Map[String, String], T],
  ): ReaderT[F, Map[String, String], P] =
    tReader.map(gen.from)

  implicit def hconsDecoder[F[_]: Functor: Semigroupal, H, T <: HList](implicit
      hReader: ReaderT[F, Map[String, String], H],
      tReader: Lazy[ReaderT[F, Map[String, String], T]],
  ): ReaderT[F, Map[String, String], H :: T] =
    ReaderT { from =>
      (hReader.run(from), tReader.value.run(from)).mapN(_ :: _)
    }
}
