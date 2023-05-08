import scala.io.*
object Day10 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis
  val start2: Long = System.currentTimeMillis

  case class Star(pos: Coord, move: Coord)
  case class Coord(x: Int, y: Int)

  private val stars: List[Star] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .map {
        case s"position=<${beginX},${beginY}> velocity=<${moveX},${moveY}>" =>
          Star(Coord(beginX.trim.toInt, beginY.trim.toInt), Coord(moveX.trim.toInt, moveY.trim.toInt))
      }

  def moveStars(stars: List[Star], seconds: Int, height: Int): Unit = {
    val movedStars: List[Star] = stars.map(s => Star(Coord(s.pos.x + s.move.x, s.pos.y + s.move.y), s.move))
    val heightDifference: Int = math.abs(movedStars.map(_.pos.y).min - movedStars.map(_.pos.y).max)
    if (heightDifference > height)
      printStarGrid(stars, seconds)
    else moveStars(movedStars, seconds + 1, heightDifference)
  }

  moveStars(stars, 0, Int.MaxValue)

  def printStarGrid(stars : List[Star], seconds: Int) = {
    val yMin: Int = stars.map(_.pos.y).min
    val yMax: Int = stars.map(_.pos.y).max
    val xMin: Int = stars.map(_.pos.x).min
    val xMax: Int = stars.map(_.pos.x).max

    println(s"Answer day $day part 1: ")
    for {
      y <- yMin to yMax
      x <- xMin to xMax
    } yield({
      if(stars.exists(s => x == s.pos.x && y == s.pos.y)) then print("#") else print(".")
      if(x == xMax) println
    })
    println(s" [${System.currentTimeMillis - start1}ms]")
    println(s"Answer day $day part 2: ${seconds} [${System.currentTimeMillis - start2}ms]")
  }


