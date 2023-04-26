import scala.io.*

object Day04 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis
  val start2: Long = System.currentTimeMillis

  private val lines: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .sorted

  case class Nap (ID : Int, start_sleep : Int, sleep_duration : Int)
  case class guard (ID : Int, total_sleep : Int)

  val pattern_guard = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)] Guard #(\\d+) begins shift".r
  val pattern_sleep = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)] falls asleep".r
  val pattern_wake = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)] wakes up".r

  def extractLines(s: String): Option[Nap] =
    s match {
      case pattern_guard(_, _, _, _, _, id) => {
        current_guard = id.toInt
        None
      }
      case pattern_sleep(_, _, _, _, minutes) => {
        start_sleep = minutes.toInt
        None
      }
      case pattern_wake(_, _, _, _, minutes) => Some(Nap(current_guard, start_sleep, minutes.toInt - start_sleep))
  }

  var current_guard = 0
  var start_sleep = 0

  val naps = lines.flatMap(l => extractLines(l))
  val grouped_naps = naps.groupBy(n => n.ID)

  val guards = grouped_naps.map(gn => guard(gn._1, gn._2.map(n => n.sleep_duration).sum))

  val sleepiest_guard = guards.maxBy(g => g.total_sleep)

  // find the most slept minute of the sleepiest guard
  var minutes = new Array[Int](60)
  grouped_naps(sleepiest_guard._1).foreach(n => {
    for(i <- 0 until minutes.size) {
      if(i >= n.start_sleep && i < n.start_sleep + n.sleep_duration) minutes(i) += 1
    }
  })
  
  var most_slept_minute = 0
  for (i <- minutes.indices) {
    if(minutes(i) == minutes.max) most_slept_minute = i
  }

  val answer1: Int = sleepiest_guard.ID * most_slept_minute
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  // PART 2
  // loop over grouped naps, put a minutes array (0-60) together for every guard. So this results in a list/map
  // of minutes arrays. Get a list of each max, and the max of those maxes

  val guards_sleep_per_minute = grouped_naps.map(gn => {
    val minutes = new Array[Int](60)
    gn._2.foreach(n => {
      for (i <- minutes.indices) {
        if (i >= n.start_sleep && i < n.start_sleep + n.sleep_duration) minutes(i) += 1
      }
    })
    (gn._1, minutes)
  })

  current_guard = guards_sleep_per_minute.maxBy(g => g._2.max)._1
  most_slept_minute = guards_sleep_per_minute(current_guard).zipWithIndex.maxBy(p => p._1)._2

  val answer2: Int = current_guard * most_slept_minute
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


