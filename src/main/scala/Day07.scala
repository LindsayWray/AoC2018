import scala.io.*

object Day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long = System.currentTimeMillis
  val start2: Long = System.currentTimeMillis

  private val rules: List[Rule] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .map {
        case s"Step ${before} must be finished before step ${after} can begin." => Rule(before.head, after.head)
      }

  case class Rule(before: Char, after: Char)
  val letterSet = rules.foldLeft(Set.empty[Char]){ case (acc, r) => acc + r.before + r.after }

  def setOrder(r: List[Rule], s: Set[Char]): Vector[Char] = {
    def go(r: List[Rule], s: Set[Char], order: Vector[Char]): Vector[Char] = {
      if(s.isEmpty) order
      else {
        val available = s.filterNot(ss => r.map(_.after).contains(ss)).min
        go(r.filterNot(e => e.before == available), s - available, order :+ available)
      }
    }
    go(r, s, Vector.empty)
  }

  val answer1 = setOrder(rules, letterSet)
  println(s"Answer day $day part 1: ${answer1.mkString} [${System.currentTimeMillis - start1}ms]")

// PART 2

  case class Worker(time: Int, letter: Char)

  def setOrder2(r: List[Rule], s: Set[Char]): Int = {
    def go(r: List[Rule], s: Set[Char], workers: List[Worker], time: Int) : Int = {
      if (s.isEmpty && workers.forall(e => e.time == 0)) time
      else if (workers.size == 5 && workers.forall(e => e.time != 0)) go(r, s, workers.map(w => Worker(w.time - 1, w.letter)), time + 1)
      else {
        val finished_workers = workers.filter(e => e.time == 0)
        val rules = r.filterNot(e => finished_workers.map(w => w.letter).contains(e.before))
        val current_workers = workers.filterNot(w => finished_workers.contains(w))

        val available = s.filterNot(ss => rules.map(_.after).contains(ss))
        if(available.nonEmpty) {
          val minLetter = available.min
          go(rules, s - minLetter, current_workers :+ Worker(minLetter - 'A' + 61, minLetter), time)
        } else {
          go(rules, s, current_workers.map(w => Worker(w.time - 1, w.letter)), time + 1)
        }
      }
    }

    go(r, s, List.empty, 0)
  }

  val answer2 = setOrder2(rules, letterSet)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")