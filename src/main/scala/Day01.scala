import scala.io._

object Day01 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  private val frequencies: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  val answer1: Int = frequencies.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def find_dup(s : Int) : Unit = {
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

  var found : Set[Int] = Set(0)
  find_dup(0)
