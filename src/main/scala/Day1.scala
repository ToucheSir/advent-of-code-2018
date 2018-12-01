import scala.io.Source._

object Day1 {
  def run() = {
    val input = fromFile("input/day1.txt").getLines.map(_.toInt).toList
    part1(input)
    part2(input)
  }

  def part1(input: Seq[Int]) = println(input.sum)

  def part2(input: Seq[Int]) = {
    // -> s1, s2, s3, ...
    val freqs = Stream.continually(input.toStream).flatten.scanLeft(0)(_ + _)
    // -> {}, {s1}, {s1, s2}, ...
    val seen = freqs.scanLeft(Set[Int]())(_ + _)
    println((seen zip freqs).filter(_ contains _).head._2)
  }
}
