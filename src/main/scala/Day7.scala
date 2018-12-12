import scala.io.Source._
import scala.collection.mutable.{Map, PriorityQueue}

import java.io._
import java.util.Arrays

object Day7 {
  val inputRegex = raw"Step (\w) must be finished before step (\w) can begin.".r

  def run() = {
    val input = fromFile("input/day7.txt").getLines.map {
      case inputRegex(s1, s2) => (s1.head.toChar, s2.head.toChar)
    }.toSeq
    // val pw = new PrintWriter(new File("steps.dot"))
    // pw.write("digraph steps {")
    // input.foreach { case (s1, s2) => pw.write(s"$s1 -> $s2;") }
    // pw.write("}")
    // pw.close()

    part1(input)
    part2(input)
  }

  def part1(input: Seq[(Char, Char)]) = println(runSteps(input, 0, 1)._2)
  def part2(input: Seq[(Char, Char)]) = println(runSteps(input, 60, 5)._1)

  def runSteps(steps: Seq[(Char, Char)], taskDuration: Int, numWorkers: Int) = {
    val forward = steps.groupBy(_._1).mapValues(_.map(_._2).toList)
    val backwards = Map.empty[Char, Int].withDefaultValue(0)
    steps.foreach(s => backwards(s._2) += 1)

    val start = forward.keySet &~ backwards.keySet
    val total = (forward.keySet | backwards.keySet).size
    val frontier = PriorityQueue.empty[Char](math.Ordering.Char.reverse) ++ start

    val workerTimes = Array.ofDim[Int](numWorkers)
    val workerItems = Array.ofDim[Char](numWorkers)

    val sb = new StringBuilder
    var consumed = 0
    var timestep = -1

    while (consumed < total) {
      val dt = workerTimes.filter(_ > 0).reduceOption(_ min _).getOrElse(1)

      for ((item, i) <- workerItems.zipWithIndex) {
        val newTime = Math.max(workerTimes(i) - dt, 0)
        if (item > 0 && newTime == 0) {
          forward.get(item).foreach { it =>
            it.foreach(backwards(_) -= 1)
            frontier ++= it.filter(backwards(_) == 0)
          }
          workerItems(i) = 0
          sb += item
          consumed += 1
        }
        workerTimes(i) = newTime
      }

      for ((item, i) <- workerItems.zipWithIndex) {
        if (item == 0 && frontier.nonEmpty) {
          val newItem = frontier.dequeue
          workerItems(i) = newItem
          workerTimes(i) = newItem - 'A' + taskDuration + 1
        }
      }

      timestep += dt
      // println(s"$timestep: ${(workerItems zip workerTimes).toVector}")
    }

    (timestep, sb.mkString)
  }
}
