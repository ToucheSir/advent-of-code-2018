import scala.io.Source._

object Day6 {
  val inputRegex = raw"(\d+), (\d+)".r

  def run() = {
    val input = fromFile("input/day6.txt").getLines.map {
      case inputRegex(x, y) => (x.toInt, y.toInt)
    }.toSeq
    val bounds = (
      input.minBy(_._1)._1,
      input.minBy(_._2)._2,
      input.maxBy(_._1)._1,
      input.maxBy(_._2)._2
    )
    println(bounds)
    part1(input, bounds)
    part2(input, bounds)
  }

  def distance(p1: (Int, Int), p2: (Int, Int)) =
    (p1._1 - p2._1).abs + (p1._2 - p2._2).abs

  def part1(input: Seq[(Int, Int)], bounds: (Int, Int, Int, Int)) = {
    val (x1, y1, x2, y2) = bounds
    val areas = (for (x <- x1 to x2; y <- y1 to y2) yield (x, y))
      .groupBy(p => {
        val distances = input.groupBy(distance(_, p))
        val minPoints = distances.minBy(_._1)._2
        if (minPoints.size > 1) None else Some(minPoints.head)
      })
      .filter {
        case (p, points) =>
          p != None && !points.exists {
            case (x, y) => x == x1 || x == x2 || y == y1 || y == y2
          }
      }
    println((x1, y1, x2, y2))
    println(areas.maxBy(_._2.size)._2.size)
  }

  def part2(input: Seq[(Int, Int)], bounds: (Int, Int, Int, Int)) = {
    val (x1, y1, x2, y2) = bounds
    val regionSize =
      (for (x <- x1 to x2; y <- y1 to y2) yield (x, y)).count(p =>
        input.map(distance(_, p)).sum < 10000)
    println(regionSize)
  }
}
