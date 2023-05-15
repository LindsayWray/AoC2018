import scala.io.*
object Day12 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  val lines: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val initialState: String = "...." + lines.head.drop(15) + "...."
  val patterns: Map[String, Char] = lines.drop(2).map(e => e.substring(0, 5) -> e.last).toMap

  def plantsOverGenerations(gen: Int, plantState: String, leftPlants: Int): (Int, Int) = {
    val newState: String = plantState.sliding(5).map(s => {
      patterns(s)
    }).mkString

    if (gen > 0) {
      plantsOverGenerations(gen - 1, "..." + newState + "...", leftPlants + 1)
    } else {
      val score1: Int = plantState.zipWithIndex.map((c, i) => c -> (i - (leftPlants + 2))).filter(_._1 == '#').foldLeft(0)(_ + _._2)
      val score2: Int = newState.zipWithIndex.map((c, i) => c -> (i - leftPlants)).filter(_._1 == '#').foldLeft(0)(_ + _._2)
      score2 - score1 -> score1
    }
  }

  val answer1: Int = plantsOverGenerations(20, initialState, 2)._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val equilibrium: (Int, Int) = plantsOverGenerations(200, initialState, 2)
  val answer2: Long = (50000000000L - 200) * equilibrium._1 + equilibrium._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")



// Initial implementation for part 1:
//
//  val day: String = this.getClass.getName.drop(3).init
//  val start1: Long = System.currentTimeMillis
//
//  val lines: List[String] =
//    Source
//      .fromResource(s"input$day.txt")
//      .getLines
//      .toList
//
//  val initialState: String = "...." + lines.head.drop(15) + "...."
//  val patterns: Map[String, Char] = lines.drop(2).map(e => e.substring(0, 5) -> e.last).toMap
//
//  def plantsOverGenerations(gen: Int, plantState: String): String = {
//    if (gen == 0) plantState
//    else {
//      val newState: String = plantState.sliding(5).map(s => {
//        patterns(s)
//      }).mkString
//      plantsOverGenerations(gen - 1, "..." + newState + "...")
//    }
//  }
//
//  val finalState: String = plantsOverGenerations(20, initialState)
//  val indexedState: List[(Char, Int)] = finalState.zipWithIndex.map(e => e._1 -> (e._2 - 24)).toList
//  val answer1: Int = indexedState.filter(e => e._1 == '#').map(_._2).sum
//
//  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")