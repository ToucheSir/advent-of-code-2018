import scala.io.Source._
import scala.collection.mutable.BitSet
import scala.math.Ordering.Implicits._

import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

enum Observation {
  case NewShift(id: Int)
  case Asleep
  case Awake
}

object Day4 {
  val inputRegex = raw"\[(\d{4}-\d\d-\d\d \d\d:\d\d)\] (.*)".r
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val newShiftRegex = raw"Guard #(\d+) begins shift".r

  // ref https://stackoverflow.com/a/50395814
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = _ compareTo _

  def run() = {
    val sortedRecords = fromFile("input/day4.txt").getLines.map {
      case inputRegex(dateTime, observation) => (LocalDateTime.parse(dateTime, dateFormatter), observation)
    }.toSeq.sortBy(_._1)

    val input = sortedRecords.view.scanLeft(0, Option.empty[Int], Option.empty[Int]) {
      case (_, (t, newShiftRegex(id))) => (id.toInt, None, None)
      case ((id, _, _), (t, "falls asleep")) => (id, Some(t.getMinute), None)
      case ((id, start, _), (t, "wakes up")) => (id, start, Some(t.getMinute))
      case (acc, _) => acc
    }.collect {
      case (id, Some(start), Some(end)) => (id, (start, end))
    }.groupBy(_._1).mapValues(_.map(_._2))

    part1(input)
    part2(input)
  }

  def part1(input: Map[Int, Seq[(Int, Int)]]) = {
    val timeSleeping = input.mapValues(_.foldLeft(0) { case (acc, (start, end)) => acc + (end - start) })
    val (id, sleepDuration) = timeSleeping.maxBy(_._2)
    val minutesAsleep = input(id).view.flatMap { case (start, end) => start until end }
    val minuteMostAsleep = minutesAsleep.groupBy(identity).maxBy(_._2.size)._1
    println(id * minuteMostAsleep)
  }

  def part2(input: Map[Int, Seq[(Int, Int)]]) = {
    val sleepMinuteFrequency = input.mapValues(_.view.flatMap {
      case (start, end) => start until end
    }.groupBy(identity).map { case (k, v) => (k, v.size) }.maxBy(_._2))
    val (id, (minuteMostAsleep, _)) = sleepMinuteFrequency.maxBy(_._2._2)
    println(id * minuteMostAsleep)
  }
}
