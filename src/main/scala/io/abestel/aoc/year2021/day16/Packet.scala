package io.abestel.aoc.year2021.day16

import cats.data.NonEmptyList
import cats.implicits._
import io.abestel.aoc.year2021.day16.Packet.Literal.SubPacket
import io.abestel.aoc.year2021.day16.Packet.Operator
import io.estatico.newtype.macros.newtype
import scodec._
import scodec.bits.BitVector
import scodec.codecs

sealed trait Packet {
  def version: Packet.Version
  def size: Long
  def value: BigInt

  def totalVersion: Long =
    this match {
      case Packet.Literal(version, _) => version.value.toLong
      case operator: Packet.Operator  => operator.version.value.toLong + operator.subPackets.map(_.totalVersion).sumAll
    }

  def prettyPrint(indentionStep: Int = 0): String = {
    val indented = (0 until (indentionStep * 2)).map(_ => " ").mkString

    this match {
      case literal @ Packet.Literal(version, subPackets) =>
        s"\n$indented${Console.YELLOW}${subPackets
          .map(_.value.toBin)
          .mkString("|")}${Console.RESET} => ${Console.GREEN}${literal.value}${Console.RESET}${Console.BLUE} @ $version${Console.RESET}"

      case operator: Packet.Operator =>
        val (operatorName, operatorSign) = operator match {
          case _: Operator.Sum         => ("Sum", " + ")
          case _: Operator.Product     => ("Product", " x ")
          case _: Operator.Minimum     => ("Minimum", " min ")
          case _: Operator.Maximum     => ("Maximum", " max ")
          case _: Operator.GreaterThan => ("GreaterThan", " > ")
          case _: Operator.LessThan    => ("LessThan", " < ")
          case _: Operator.Equal       => ("Equal", " == ")
        }

        val prettyPackets = operator.subPackets.map(_.prettyPrint(indentionStep + 1)).mkString_(",")
        val values        = operator.subPackets.map(_.value).mkString_(operatorSign)
        val versions      = (operator.subPackets.map(_.totalVersion) prepend operator.version.value.toLong).mkString_("(", " + ", ")")
        s"\n$indented$operatorName${Console.BLUE} @ ${operator.version} ${Console.RESET}($prettyPackets\n$indented) => ${Console.GREEN}($values) = ${operator.value}${Console.RESET} ${Console.BLUE}@ $versions = ${operator.totalVersion}${Console.RESET}"
    }
  }
}

object Packet {
  case class Literal(version: Version, subPackets: List[SubPacket]) extends Packet {
    override def value: BigInt =
      subPackets
        .map(_.value)
        .fold(BitVector.empty)(_ ++ _)
        .toIndexedSeq
        .reverseIterator
        .foldLeft((BigInt(0), BigInt(1L))) { case ((agg, powOf2), currentBit) =>
          if (currentBit) {
            (
              agg + powOf2,
              powOf2 * 2,
            )
          } else {
            (
              agg,
              powOf2 * 2,
            )
          }
        }
        ._1

    override def size: Long = /* version */ 3L + /* typeId */ 3L + /* number of subPackets*/ subPackets.size.toLong * /* literal */ 5L
  }

  object Literal {
    case class SubPacket(hasNext: Boolean, value: BitVector)

    object SubPacket {
      implicit val subPacketCodec: Codec[SubPacket] =
        (codecs.bool ~ codecs.bits(4)).xmap(
          { case (hasNext, value) => SubPacket(hasNext, value) },
          subPacket => subPacket.hasNext -> subPacket.value,
        )

    }
  }

  sealed trait Operator extends Packet {
    override def size: Long = /* version */ 3L + /* typeId */ 3L + /* lengthTypeId */ 1L + lengthTypeId.dataSize.toLong + subPackets.map(_.size).sumAll

    def lengthTypeId: LengthTypeId
    def subPackets: NonEmptyList[Packet]
  }

  object Operator {
    sealed abstract class ListOperator(op: NonEmptyList[BigInt] => BigInt) extends Operator {
      override def value: BigInt = op(subPackets.map(_.value))
    }

    case class Sum(version: Version, lengthTypeId: LengthTypeId, subPackets: NonEmptyList[Packet])     extends ListOperator(_.sumAll)
    case class Product(version: Version, lengthTypeId: LengthTypeId, subPackets: NonEmptyList[Packet]) extends ListOperator(_.productAll)
    case class Minimum(version: Version, lengthTypeId: LengthTypeId, subPackets: NonEmptyList[Packet]) extends ListOperator(_.minimum)
    case class Maximum(version: Version, lengthTypeId: LengthTypeId, subPackets: NonEmptyList[Packet]) extends ListOperator(_.maximum)

    sealed abstract class TupleOperator(op: (BigInt, BigInt) => Boolean) extends Operator {
      def subPacket1: Packet
      def subPacket2: Packet
      override def subPackets: NonEmptyList[Packet] = NonEmptyList.of(subPacket1, subPacket2)

      override def value: BigInt =
        if (op(subPacket1.value, subPacket2.value)) {
          1
        } else {
          0
        }
    }

    case class GreaterThan(version: Version, lengthTypeId: LengthTypeId, subPacket1: Packet, subPacket2: Packet) extends TupleOperator(_ > _)
    case class LessThan(version: Version, lengthTypeId: LengthTypeId, subPacket1: Packet, subPacket2: Packet)    extends TupleOperator(_ < _)
    case class Equal(version: Version, lengthTypeId: LengthTypeId, subPacket1: Packet, subPacket2: Packet)       extends TupleOperator(_ == _)
  }

  @newtype case class Version(value: Int)
  implicit val versionCodec: Codec[Version] = codecs.uint(bits = 3).xmap(Version(_), _.value)

  sealed trait TypeId {
    def rawValue: Int
  }

  object TypeId {
    case object LiteralTypeId extends TypeId {
      override def rawValue: Int = 4
    }

    sealed trait OperatorTypeId extends TypeId

    sealed abstract class ListOperatorId(override val rawValue: Int) extends OperatorTypeId
    case object SumTypeId                                            extends ListOperatorId(rawValue = 0)
    case object ProductTypeId                                        extends ListOperatorId(rawValue = 1)
    case object MinimumTypeId                                        extends ListOperatorId(rawValue = 2)
    case object MaximumTypeId                                        extends ListOperatorId(rawValue = 3)

    sealed abstract class TupleOperatorId(override val rawValue: Int) extends OperatorTypeId
    case object GreaterThanTypeId                                     extends TupleOperatorId(rawValue = 5)
    case object LessThanTypeId                                        extends TupleOperatorId(rawValue = 6)
    case object EqualTypeId                                           extends TupleOperatorId(rawValue = 7)

    val values: List[TypeId] = List(
      LiteralTypeId,
      SumTypeId,
      ProductTypeId,
      MinimumTypeId,
      MaximumTypeId,
      GreaterThanTypeId,
      LessThanTypeId,
      EqualTypeId,
    )

    val rawValueToType: Map[Int, TypeId] =
      values.map(typeId => typeId.rawValue -> typeId).toMap

  }

  implicit val typeIdCodec: Codec[TypeId] = codecs
    .uint(bits = 3)
    .exmap(
      rawValue =>
        TypeId.rawValueToType
          .get(rawValue)
          .fold[Attempt[TypeId]](
            Attempt.failure(Err.MatchingDiscriminatorNotFound(rawValue, List.empty))
          )(Attempt.successful),
      typeId => Attempt.successful(typeId.rawValue),
    )

  @newtype case class LengthTypeId(value: Boolean) {
    def isNumberOfPackets: Boolean = value
    def isNumberOfBits: Boolean    = !value

    def dataSize: Int =
      if (isNumberOfPackets) {
        11
      } else {
        15
      }
  }
  implicit val lengthTypeIdCodec: Codec[LengthTypeId] = codecs.bool.xmap(LengthTypeId(_), _.value)

  private def readSubPackets[T](
      bits: BitVector
  )(
      stop: List[T] => Boolean
  )(implicit decoder: Decoder[T]): Attempt[DecodeResult[List[T]]] = {
    def go(
        remaining: BitVector,
        result: List[T] = List.empty,
    ): Attempt[DecodeResult[List[T]]] = {
      decoder.decode(remaining).flatMap { case DecodeResult(current, newRemaining) =>
        val newResult = result :+ current
        if (stop(newResult)) {
          Attempt.successful(
            DecodeResult(
              newResult,
              newRemaining,
            )
          )
        } else {
          go(
            remaining = newRemaining,
            result = newResult,
          )
        }
      }
    }

    go(bits)
  }

  implicit lazy val decoder: Decoder[Packet] = Decoder { bits =>
    for {
      DecodeResult(version, bits) <- versionCodec.decode(bits)
      DecodeResult(typeId, bits)  <- typeIdCodec.decode(bits)
      DecodeResult(packet, bits) <- typeId match {
        case TypeId.LiteralTypeId =>
          readSubPackets[Literal.SubPacket](bits)(_.lastOption.exists(p => !p.hasNext))
            .map { case DecodeResult(subPackets, bits) =>
              DecodeResult(Literal(version, subPackets), bits)
            }

        case operatorTypeId: TypeId.OperatorTypeId =>
          for {
            DecodeResult(lengthTypeId, bits)      <- lengthTypeIdCodec.decode(bits)
            DecodeResult(decodedDataLength, bits) <- codecs.uint(lengthTypeId.dataSize).decode(bits)
            DecodeResult(subPackets, bits) <-
              if (lengthTypeId.isNumberOfBits) {
                readSubPackets[Packet](bits)(_.map(_.size).sum == decodedDataLength)
              } else {
                readSubPackets[Packet](bits)(_.size == decodedDataLength)
              }
            result <- operatorTypeId match {
              case listOperator: TypeId.ListOperatorId =>
                subPackets.toNel match {
                  case None =>
                    Attempt.failure(Err.General(s"SubPackets list should be nonEmpty for operator $listOperator", List.empty))

                  case Some(subPacketsNel) =>
                    Attempt.successful(
                      DecodeResult(
                        listOperator match {
                          case TypeId.SumTypeId     => Operator.Sum(version, lengthTypeId, subPacketsNel)
                          case TypeId.ProductTypeId => Operator.Product(version, lengthTypeId, subPacketsNel)
                          case TypeId.MinimumTypeId => Operator.Minimum(version, lengthTypeId, subPacketsNel)
                          case TypeId.MaximumTypeId => Operator.Maximum(version, lengthTypeId, subPacketsNel)
                        },
                        bits,
                      )
                    )
                }

              case tupleOperator: TypeId.TupleOperatorId =>
                subPackets match {
                  case first :: second :: Nil =>
                    Attempt.successful(
                      DecodeResult(
                        tupleOperator match {
                          case TypeId.GreaterThanTypeId => Operator.GreaterThan(version, lengthTypeId, first, second)
                          case TypeId.LessThanTypeId    => Operator.LessThan(version, lengthTypeId, first, second)
                          case TypeId.EqualTypeId       => Operator.Equal(version, lengthTypeId, first, second)
                        },
                        bits,
                      )
                    )

                  case _ =>
                    Attempt.failure(Err.General(s"SubPackets list should have exactly two elements for operator $tupleOperator", List.empty))
                }
            }
          } yield result

      }
    } yield DecodeResult(
      packet,
      bits,
    )
  }
}
