package io.abestel.aoc.utils

import fs2.io.file.{Files, Path}
import fs2.{text, Stream}

trait Fs2FilesExt {
  def resourceStream[F[_]: Files](fileName: String): Stream[F, String] =
    Files[F]
      .readAll(
        Path(
          getClass.getClassLoader
            .getResource(fileName)
            .getPath
        )
      )
      .through(text.utf8.decode)
      .through(text.lines)
}

object Fs2FilesExt extends Fs2FilesExt
