import scala.io.Source._

object Day5 {
  def run() = {
    val input = fromFile("input/day5.txt").getLines.next
    part1(input)
    part2(input)
  }
  
  def react(polymer: String) = polymer.foldLeft(List.empty[Char]) {
    case (head :: tail, c) if head != c && head.toLower == c.toLower => tail
    case (list, c) => c :: list
  }

  def part1(input: String) = println(react(input).size)

  def part2(input: String) = println(
    ('a' to 'z').map(c => react(input.replaceAll(s"[$c${c.toUpper}]", "")).size).min
  )
}
