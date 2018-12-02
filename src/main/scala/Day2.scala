import scala.io.Source._

object Day2 {
  def run() = {
    val input = fromFile("input/day2.txt").getLines.toList
    part1(input)
    part2(input)
  }

  def part1(input: Seq[String]) = {
    val counts = input.map(_.toArray.groupBy(identity).values.map(_.size))
    val hasRepeats = counts.map(c => (c.exists(2.==), c.exists(3.==)))
    val sums = hasRepeats.foldLeft((0, 0))((a, b) => (a._1 + b._1.compare(false), a._2 + b._2.compare(false)))
    println(sums._1 * sums._2)
  }

  def part2(input: Seq[String]) = {
    val matchingLength = input.head.size - 1
    val combs = input.combinations(2).map(pair => (pair.head zip pair.last))
    val commonLetters = combs.map(_.filter((c1, c2) => c1 == c2).map(_._1).mkString)
    println(commonLetters.filter(_.size == matchingLength).next)
  }
}
