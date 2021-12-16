package io.abestel.aoc.year2020.day7

case class BagRules(
    rules: Map[BagName, BagRule]
) {
  def contains(bagName: BagName, target: BagName): Boolean = {
    def loop(
        currentBag: BagName
    ): Boolean =
      rules.getOrElse(currentBag, BagRule.Empty) match {
        case BagRule.Contains(rules) =>
          rules.iterator.exists { case (thatBag, _) =>
            thatBag == target || loop(thatBag)
          }

        case BagRule.Empty => false
      }

    loop(bagName)
  }

  def capacity(bagName: BagName): Int = {
    def loop(name: BagName = bagName): Int =
      rules.getOrElse(name, BagRule.Empty) match {
        case BagRule.Empty           => 1
        case BagRule.Contains(rules) => 1 + rules.iterator.map { case (thatName, number) => loop(thatName) * number }.sum
      }

    loop() - 1
  }
}
