package io.abestel.aoc.utils.geom

import scala.math._

// format: off
case class IntMatrix3 (
  `0,0`: Int,`0,1`: Int,`0,2`: Int,
  `1,0`: Int,`1,1`: Int,`1,2`: Int,
  `2,0`: Int,`2,1`: Int,`2,2`: Int,
) {
  override def toString: String =
    s"""~|\t${`0,0`},\t${`0,1`},\t${`0,2`}\t|
       ~|\t${`1,0`},\t${`1,1`},\t${`1,2`}\t|
       ~|\t${`2,0`},\t${`2,1`},\t${`2,2`}\t|""".stripMargin('~')
}

object IntMatrix3 {
  def rotationXYZ(
    halfPisX: Int,
    halfPisY: Int,
    halfPisZ: Int,
  ): IntMatrix3 = {
    val alpha = Pi / 2 * halfPisZ
    val beta = Pi / 2 * halfPisY
    val gamma = Pi / 2 * halfPisX

    IntMatrix3(
      (cos(alpha) * cos(beta)).toInt, (cos(alpha) * sin(beta) * sin(gamma) - sin(alpha) * cos(gamma)).toInt, (cos(alpha) * sin(beta) * cos(gamma) + sin(alpha) * sin(gamma)).toInt,
      (sin(alpha) * cos(beta)).toInt, (sin(alpha) * sin(beta) * sin(gamma) + cos(alpha) * cos(gamma)).toInt, (sin(alpha) * sin(beta) * cos(gamma) - cos(alpha) * sin(gamma)).toInt,
      (- sin(beta)).toInt,            (cos(beta) * sin(gamma)).toInt,                                        (cos(beta) * cos(gamma)).toInt,
    )
  }

  val rotation90degreesAllAngles: List[IntMatrix3] =
    (for {
      xPi <- 0 to 3
      yPi <- 0 to 3
      zPi <- 0 to 3
    } yield {
      rotationXYZ(xPi, yPi, zPi)
    }).toList.distinct
}
// format: on
