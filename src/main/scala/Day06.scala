import scala.io.*
object Day06 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis


  private val locations: List[Location] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .map {
        case s"${x}, ${y}" => Location(x.toInt, y.toInt)
      }

  case class Location(x: Int, y: Int)
  def findNearest(loc: List[Location], x: Int, y: Int) : Option[Location] = {
    val distances = loc.map(l => math.abs(x - l.x) + math.abs(y - l.y)).zip(loc)
    val nearest = distances.minBy(e => e._1)

    if(distances.count(d => d._1 == nearest._1) > 1) None
    else Some(nearest._2)
  }

  val min_x = locations.map(e => e.x).min
  val min_y = locations.map(e => e.y).min
  val max_x = locations.map(e => e.x).max
  val max_y = locations.map(e => e.y).max

  val coord = for {
    y <- min_y to max_y
    x <- min_x to max_x
  } yield (x, y)

  val coord_with_locations = coord.flatMap(c => findNearest(locations, c._1, c._2).map(l => (l, c)))
  val grouped_by_loc = coord_with_locations.groupBy(e => e._1)
  val filtered_sides = grouped_by_loc.filterNot(e => e._2.exists(p => p._2._1 == min_x || p._2._1 == max_x || p._2._2 == min_y || p._2._2 == max_y))

  val answer1: Int = filtered_sides.map(e => e._2.size).max
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  // *** Part 2 ***
  val start2: Long = System.currentTimeMillis
  def partOfRegion(loc: List[Location], x: Int, y: Int): Int = {
    loc.map(l => math.abs(x - l.x) + math.abs(y - l.y)).sum
  }

  val total_distances = coord.map(c => partOfRegion(locations, c._1, c._2))
  val answer2: Int = total_distances.count(e => e < 10000)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

