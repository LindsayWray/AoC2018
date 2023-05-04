import scala.annotation.tailrec
import scala.io.*

object Day05 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  private val polymer: String =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .mkString

  def reduceString(str: String): String = {
    def go(s: String, prefix : String = ""): String = {
      def go2(i : Int) : Option[Int] = {
        if(i >= s.length - 1) None
        else if (s(i) == s(i + 1) + 32 || s(i) == s(i + 1) - 32) Some(i) // ascii difference between upper/lower
        else go2(i + 1)
      }
      go2(0) match
        case Some(i) => go(s.substring(i + 2), prefix + s.substring(0, i))
        case None => prefix + s
    }
    val result = go(str)
    if(result == str) result
    else reduceString(result)
  }

  val answer1: Int = reduceString(polymer).length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  // Part 2
  private val start2: Long = System.currentTimeMillis

  val answer2 = ('a' to 'z').map(c => {
    reduceString(polymer.filterNot(letter => letter.toLower == c)).length
  }).min

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")



