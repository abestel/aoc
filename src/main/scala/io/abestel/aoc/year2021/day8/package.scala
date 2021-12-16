package io.abestel.aoc.year2021

import cats.ApplicativeError
import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import fs2.io.file.Files
import io.abestel.aoc.utils.Fs2FilesExt

import scala.annotation.tailrec

package object day8 {

  def dataStream[F[_]: Files: Sync]: Stream[F, List[Number]] =
    Fs2FilesExt
      .resourceStream[F]("2021/day8.txt")
      .evalMap { inputLine =>
        inputLine.split("\\|") match {
          case Array(input, output) =>
            (
              input.split(" ").toList.filterNot(_.isEmpty).traverse(Segments.parse[F]),
              output.split(" ").toList.filterNot(_.isEmpty).traverse(Segments.parse[F]),
            ).tupled

          case other =>
            ApplicativeError[F, Throwable].raiseError[(List[Segments], List[Segments])](
              new IllegalArgumentException(s"'${other.mkString("|")}' is not a valid input")
            )
        }
      }
      .evalMap { case (input, output) =>
        decode[F](input, output)
      }

  implicit class IterableTupleGroupSetOps[A, B](private val iterable: Iterable[(A, B)]) extends AnyVal {
    def groupTupleToSet: Map[A, Set[B]] =
      iterable.groupMapReduce(_._1) { case (_, b) => Set(b) }(_ union _)
  }

  /**
   * Decode the output into a list of numbers
   *
   * @return
   */
  def decode[F[_]](input: List[Segments], output: List[Segments])(implicit F: ApplicativeError[F, Throwable]): F[List[Number]] = {
    @tailrec
    def go(dictionary: Dictionary): Option[List[Number]] = {
      output.traverse { segments =>
        dictionary.decode(segments) match {
          case onlyOne :: Nil => Some(onlyOne)
          case _              => None
        }
      } match {
        case success @ Some(_) => success

        case None =>
          val segmentToNumber = (input ++ output).map(segments => segments -> dictionary.decode(segments))

          val newDictionary =
            segmentToNumber
              .foldLeft(dictionary) { case (newDictionary, thisCase) =>
                thisCase match {
                  case (Segments(inputSegments), Number(outputSegments) :: Nil) if inputSegments.size == outputSegments.size =>
                    newDictionary.restrict(inputSegments, outputSegments)

                  case _ =>
                    newDictionary
                }
              }

          if (newDictionary != dictionary) {
            go(newDictionary)
          } else {
            None
          }
      }
    }

    F.fromOption(
      go(Dictionary.empty),
      new IllegalStateException(s"The output $output with input $input could not be decoded"),
    )
  }
}
