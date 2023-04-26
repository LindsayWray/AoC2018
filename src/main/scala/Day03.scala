import com.sun.jdi.BooleanValue

import scala.io.*

object Day03 extends App:

  case class claim (ID : Int, left : Int, top : Int, width : Int, height : Int)

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  val lines: Vector[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toVector

  val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  def extractLines(s : String) : claim =
    s match {
      case pattern(id, left, top, width, height) => {
        claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
      }
  }

  val claims = lines.map(l => extractLines(l))
  
  val width = claims.map(c => c.left + c.width).max
  val height = claims.map(c => c.top + c.height).max

  var claimed = 0
  var total = 0
  for(y <- 0 to height) {
    for(x <- 0 to width){
      claims.foreach(c => {
        if (x >= c.left && x < c.left + c.width && y >= c.top && y < c.top + c.height) {
          claimed += 1
        }
      })
      if (claimed > 1) total += 1
      claimed = 0
    }
  }

  println(s"Answer day $day part 1: ${total} [${System.currentTimeMillis - start1}ms]")


// PART 2
  val start2: Long = System.currentTimeMillis
  def findOverlap(cl : claim, cr : claim) : Boolean = {
    if(cl.ID == cr.ID) false
    else if(cl.left + cl.width < cr.left || cr.left + cr.width < cl.left) false
    else if(cl.top + cl.height < cr.top || cr.top + cr.height < cl.top) false
    else true
  }

  var uniqueClaim = 0
  claims.foreach(cl => {
    if (claims.forall(cr => !findOverlap(cl, cr))) uniqueClaim = cl.ID
  })

  println(s"Answer day $day part 2: ${uniqueClaim} [${System.currentTimeMillis - start2}ms]")


