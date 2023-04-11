import scala.io._

object Day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val frequencies: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  val answer1: Int = frequencies.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private def find_dup(s : Int) : Unit = {
    var dup : Boolean = true
    var state = s;
    frequencies.foreach(f => {
      state += f
      if (found.contains(state) && dup)
        println(s"Answer day $day part 2: ${state} [${System.currentTimeMillis - start1}ms]")
        dup = false
      else if (dup)
        found += (state)
    })
    if (dup) find_dup(state)
  }

  private var found : Set[Int] = Set(0)
  find_dup(0)
