package io.abestel.aoc.year2020

import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day4 {
  def dataStream[F[_]: Files]: Stream[F, Map[String, String]] =
    Fs2FilesExt
      .resourceStream("2020/day4.txt")
      .groupAdjacentBy(_.nonEmpty)
      .collect {
        case (nonEmpty, chunks) if nonEmpty =>
          (for {
            chunk     <- chunks.toArray
            chunkPart <- chunk.split(" ")
            keyValues <-
              chunkPart.split(":") match {
                case Array(key, value) =>
                  Some(key -> value)

                case _ =>
                  None
              }
          } yield keyValues).toList.toMap
      }

}
