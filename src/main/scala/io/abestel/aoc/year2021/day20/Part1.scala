package io.abestel.aoc.year2021.day20

import cats.effect.{IO, IOApp}

object Part1 extends IOApp.Simple {
  override def run: IO[Unit] =
    data[IO].map { case (algo, image) =>
      val (newImage, _) = Image.steps(algo)(2).run(image).value
      println(newImage.visualize)
      println(newImage.values.count(_._2 == Pixel.Light))
    }
}
