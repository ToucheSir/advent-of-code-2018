import scala.io.Source._
import scala.collection.mutable.BitSet

object Day3 {
  val inputRegex = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r

  def run() = {
    val input = fromFile("input/day3.txt").getLines.map {
      _ match {
        case inputRegex(id, x, y, w, h) => (id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }
    }.toList
    val overlaps = part1(input)
    part2(input, overlaps)
  }

  def part1(input: Seq[(Int, Int, Int, Int, Int)]): BitSet = {
    val coords = input.flatMap(_ match {
      case (id, x, y, w, h) => for (i <- x to x + w - 1; j <- y to y + h - 1) yield 1000 * i + j
    })
    val filled = new BitSet(1000 * 1000)
    val filled2 = new BitSet(1000 * 1000)
    for (c <- coords) {
      (if (filled contains c) then filled2 else filled) += c
    }
    println(filled2.size)
    return filled2
  }

  def part2(input: Seq[(Int, Int, Int, Int, Int)], overlaps: BitSet) = {
    val res = input.filter(_ match {
      case (id, x, y, w, h) => (
        for (i <- x to x + w - 1; j <- y to y + h - 1) yield 1000 * i + j
      ).forall(c => !(overlaps contains c))
    })
    println(res.head._1)
  }
}
