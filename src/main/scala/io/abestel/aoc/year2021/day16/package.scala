package io.abestel.aoc.year2021

import cats.effect.Sync
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt
import scodec.{DecodeResult, Decoder}
import scodec.bits._

package object day16 {
  def data[F[_]: Files: Sync]: F[List[(String, DecodeResult[Packet])]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day16.txt")
      .evalMap { raw =>
        for {
          hexa <- Sync[F].fromOption(
            BitVector.fromHex(raw),
            new IllegalArgumentException(s"'$raw' is not a valid hexadecimal string"),
          )

          packet <- Sync[F].fromEither(
            Decoder[Packet]
              .decode(hexa)
              .toEither
              .leftMap(err => new IllegalStateException(err.messageWithContext))
          )
        } yield raw -> packet
      }
      .compile
      .toList
}
