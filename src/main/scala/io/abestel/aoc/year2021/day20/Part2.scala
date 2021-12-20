package io.abestel.aoc.year2021.day20

import cats.effect.{IO, IOApp}

object Part2 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { case (algo, image) =>
      println(image.visualize)

      val newImage = Image.steps(algo)(50).runS(image).value
      println(newImage.visualize)

      println(newImage.values.count(_._2 == Pixel.Light))
    }
}
