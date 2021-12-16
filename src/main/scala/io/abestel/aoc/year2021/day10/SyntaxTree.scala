package io.abestel.aoc.year2021.day10

import cats.Show
import cats.syntax.show._

import scala.annotation.tailrec

sealed trait SyntaxTree

object SyntaxTree {
  case object Empty                                                      extends SyntaxTree
  case class Opening[O <: Opener[C], C <: Closer](index: Int, symbol: O) extends SyntaxTree { def closer: C = symbol.closer }
  case class Block[O <: Opener[C], C <: Closer](opening: Opening[O, C], subBlocks: List[SyntaxTree], closer: C, closingIndex: Int) extends SyntaxTree

  implicit lazy val showSyntaxTree: Show[SyntaxTree] =
    Show.show {
      case Empty                                => "empty"
      case Opening(_, symbol)                   => symbol.entryName
      case Block(opening, subBlocks, closer, _) => s"${opening.symbol.entryName}${subBlocks.map(_.show).mkString("")}${closer.entryName}"
    }

  sealed abstract class Error extends Exception

  object Error {
    case class SyntaxError(
        program: List[Character],
        invalidCharacter: Character,
        atPosition: Int,
        expected: String,
    ) extends Error {
      override def getMessage: String =
        s"Invalid character at position $atPosition, expected '$expected' but got '$invalidCharacter'"
    }

    case class Incomplete(
        program: List[Character],
        opener: Opener[_ <: Closer],
        openedAt: Int,
        missingAtPosition: Int,
        stack: List[SyntaxTree],
    ) extends Error {
      override def getMessage: String =
        s"Incomplete program at position $missingAtPosition, expected '${opener.closer}' matching '$opener' at position $openedAt but got EOL"
    }
  }

  def parser(program: List[Character]): Error Either SyntaxTree = {
    @tailrec
    def go(
        index: Int = 0,
        remaining: List[Character] = program,
        stack: List[SyntaxTree] = List.empty,
        syntaxTree: SyntaxTree = Empty,
    ): Error Either SyntaxTree =
      remaining match {
        case Nil =>
          stack match {
            case Nil =>
              Right(syntaxTree)

            case nonEmpty =>
              nonEmpty
                .collectFirst { case Opening(openingIndex, symbol) => Left(Error.Incomplete(program, symbol, openingIndex, index, syntaxTree +: stack)) }
                .getOrElse(throw new IllegalStateException("This should really not happen"))

          }

        case head :: tail =>
          syntaxTree match {
            case Empty =>
              head match {
                case closer: Closer    => Left(Error.SyntaxError(program, closer, index, Character.openers.mkString(",")))
                case opener: Opener[c] => go(index + 1, tail, stack, Opening[opener.type, c](index, opener))
              }

            case block @ Block(_, _, _, _) =>
              head match {
                case opener: Opener[c] =>
                  go(index + 1, tail, block +: stack, Opening[opener.type, c](index, opener))

                case closer: Closer =>
                  findFirstOpening(stack) match {
                    case Some((subBlocks, opening: Opening[o, c], newStack)) =>
                      if (closer == opening.symbol.closer) {
                        go(index + 1, tail, newStack, Block[o, c](opening, subBlocks :+ syntaxTree, opening.closer, index))
                      } else {
                        Left(Error.SyntaxError(program, closer, index, opening.closer.entryName))
                      }

                    case None =>
                      Left(Error.SyntaxError(program, closer, index, Character.openers.mkString(",")))
                  }
              }

            case opening: Opening[_, c] =>
              head match {
                case validCloser: c      => go(index + 1, tail, stack, Block(opening, List.empty, validCloser, index))
                case wrongCloser: Closer => Left(Error.SyntaxError(program, wrongCloser, index, opening.closer.entryName))
                case opener: Opener[c]   => go(index + 1, tail, opening +: stack, Opening[opener.type, c](index, opener))
              }
          }
      }

    go()
  }

  def findFirstOpening(
      stack: List[SyntaxTree]
  ): Option[(List[SyntaxTree], Opening[_ <: Opener[_], _ <: Closer], List[SyntaxTree])] = {
    @tailrec
    def go(
        subBlocks: List[SyntaxTree] = List.empty,
        remaining: List[SyntaxTree] = stack,
    ): Option[(List[SyntaxTree], Opening[_ <: Opener[_], _ <: Closer], List[SyntaxTree])] =
      remaining match {
        case Nil => None
        case head :: tail =>
          head match {
            case opening @ Opening(_, _) => Some((subBlocks, opening, tail))
            case other                   => go(other :: subBlocks, tail)
          }
      }

    go()
  }
}
