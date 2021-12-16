package io.abestel.aoc.utils.shapeless

import cats.{Functor, Semigroupal}
import cats.data.ReaderT
import cats.implicits._
import shapeless.{::, HList, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

trait ReaderTDerivationInstances extends ReaderTDerivationInstances1 {
  implicit def caseClassDecoder[F[_]: Functor, P, T](implicit
      labeledGen: LabelledGeneric.Aux[P, T],
      tReader: Lazy[ReaderT[F, Map[String, String], T]],
  ): ReaderT[F, Map[String, String], P] =
    tReader.value.map(labeledGen.from)

  implicit def hkCons[F[_]: Functor: Semigroupal, K <: Symbol, H, T <: HList](implicit
      key: Witness.Aux[K],
      headerValueReader: ReaderT[F, Option[String], H],
      tReader: Lazy[ReaderT[F, Map[String, String], T]],
  ): ReaderT[F, Map[String, String], FieldType[K, H] :: T] =
    ReaderT { from =>
      val value = from.get(key.value.name)
      val head  = headerValueReader.run(value).map(field[K](_))
      val tail  = tReader.value.run(from)
      (head, tail).mapN(_ :: _)
    }
}
