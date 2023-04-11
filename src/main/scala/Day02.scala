import scala.io._

case class ID (var two : Boolean, var three : Boolean)

object Day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start2: Long =
    System.currentTimeMillis

  val lines : List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

//  for (elem <- lines)
//    println(elem + " ")

  def findDups(id : String): ID = {
    val id_data: ID = new ID(false, false)
    for(i <- 0 until id.size) {
      if(id.count(_ == id(i)) >= 3) {
          id_data.three = true
      } else if (id.count(_ == id(i)) >= 2 ) {
        id_data.two = true
      }
    }
    id_data
  }

  var IDs : Vector[ID] = Vector.empty
  lines.foreach(id => IDs :+= findDups(id))

  var twos : Int = 0
  var threes : Int = 0
  IDs.foreach(id => {
    if(id.two)  twos += 1
    if(id.three) threes += 1
  })

  val answer1: Int = twos * threes
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start2}ms]")



  // PART 2
  def strCompare(l : String, r : String) : Boolean= {
    var ret : Boolean = true
    var diff : Boolean = false
    for(i <- 0 until l.size) {
      if (l(i) != r(i) && diff)  ret = false
      else if (l(i) != r(i)) diff = true
    }
    ret
  }

  def compare(str : String, lst : List[String]) : Option[(String, String)] = {
    var ret : Option[(String, String)] = None
    lst.foreach(s => {
      if(strCompare(str, s)) ret = Some((str, s))
    })
    ret
  }

  def findMatch(lst : List[String]) : Option[(String, String)] = {
    lst match
      case h :: t => {
        val ret = compare(h, t)
        if (ret.isEmpty) findMatch(t)
        else ret
      }
      case Nil => None
  }


  val pair = findMatch(lines)
  var answer2 : String = ""

  for (i <- 0 until pair.get._1.size) {
    if (pair.get._1(i) == pair.get._2(i)) answer2 += pair.get._1(i)
  }

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]") // 83173


