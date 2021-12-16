package io.abestel.aoc.year2021

import cats.data.NonEmptyList
import cats.{Applicative, ApplicativeError}
import cats.effect.Async
import cats.effect.implicits._
import cats.implicits._
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

package object day14 {
  def data[F[_]: Files: Async]: F[(NonEmptyList[Char], Map[(Char, Char), Char])] = {
    val dataStream = Fs2FilesExt.resourceStream[F]("2021/day14.txt")

    val template = dataStream.head
      .evalMap { template =>
        ApplicativeError[F, Throwable].fromOption(
          template.toList.toNel,
          new IllegalArgumentException("The template should be non empty"),
        )
      }
      .compile
      .lastOrError

    val recipes = dataStream.tail
      .filter(_.nonEmpty)
      .evalScan(Map.empty[(Char, Char), Char]) { case (resultMap, rawRecipe) =>
        rawRecipe match {
          case s"$pattern -> $insert" if pattern.length == 2 && insert.length == 1 =>
            Applicative[F].pure(
              resultMap.updated(pattern(0) -> pattern(1), insert(0))
            )

          case other =>
            ApplicativeError[F, Throwable].raiseError(
              new IllegalArgumentException(s"'$other' is not a valid input")
            )
        }
      }
      .compile
      .lastOrError

    (template, recipes).parTupled
  }

  def polymerize(template: NonEmptyList[Char], recipes: Map[(Char, Char), Char])(steps: Int): Unit = {
    val patterns = template.toList.zip(template.tail).groupMapReduce(identity)(_ => 1L)(_ + _)
    println(s"Template:\n${patterns.toList.sortBy(_._1).mkString("\n")}\n")

    val expanded = (0 until steps).foldLeft(patterns) { case (patterns, step) =>
      val newPatterns = patterns.foldLeft(
        Map.empty[(Char, Char), Long]
      ) { case (newPatternsCount, (pattern @ (firstChar, secondChar), patternCount)) =>
        newPatternsCount |+| recipes
          .get(pattern)
          .fold(
            Map(pattern -> patternCount)
          ) { insert =>
            Map(
              (firstChar, insert)  -> patternCount,
              (insert, secondChar) -> patternCount,
            )
          }
      }

      println(s"After step ${step + 1}:\n${newPatterns.toList.sortBy(_._1).mkString("\n")}\n")
      newPatterns
    }

    val lastCharacter = Map(template.last -> 1L) // the last character never changes
    val occurences    = lastCharacter |+| expanded.toList.groupMapReduce { case ((c, _), _) => c } { case (_, count) => count }(_ + _)

    println(occurences.toList.sortBy(_._1).mkString("\n"))

    val (_, minOcc) = occurences.minBy { case (_, occ) => occ }
    val (_, maxOcc) = occurences.maxBy { case (_, occ) => occ }

    println(maxOcc - minOcc)
  }
}
