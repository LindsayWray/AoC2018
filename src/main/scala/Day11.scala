import scala.io.*
object Day11 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  val serial: Int =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .mkString
      .toInt

  case class Cell(x: Int, y: Int, power: Int)
  val grid = (0 to 300).map(y => (0 to 300).map(x => Cell(x, y, calculatePower(x, y))))

  def calculatePower(x: Int, y: Int): Int = {
    ((((((x + 10) * y) + serial) * (x + 10)) % 1000) / 100) - 5 // 🤡
  }

  val answer1: (Int, Int, Int) = (for {
      y <- 2 until 299
      x <- 2 until 299
  } yield({
    (x - 1, y - 1, grid(y)(x).power + grid(y - 1)(x).power + grid(y + 1)(x).power + grid(y)(x - 1).power + grid(y)(x + 1).power +
      grid(y - 1)(x - 1).power + grid(y + 1)(x + 1).power + grid(y - 1)(x + 1).power + grid(y + 1)(x - 1).power)
  })).maxBy(_._3)

  println(s"Answer day $day part 1: ${answer1._1},${answer1._2} (power level: ${answer1._3}) [${System.currentTimeMillis - start1}ms]")


  //Part 2
  val start2: Long = System.currentTimeMillis

  def calculateSquares(x: Int, y: Int, size: Int, lst: List[(Int, Int)]): List[(Int, Int)] = {
    if(size + x >= 300 || size + y >= 300) lst
    else {
      val squareSum = lst.head._2 + calcColumn(x + size, y, y + size) + calcRow(x, y + size, x + size)
      calculateSquares(x, y, size + 1, (size, squareSum) +: lst)
    }
  }

  def calcColumn(x: Int, y: Int, n: Int, acc: Int = 0): Int = {
    if (y > n) acc
    else calcColumn(x, y + 1, n, acc + grid(y)(x).power)
  }

  def calcRow(x: Int, y: Int, n: Int, acc: Int = 0): Int = {
    if (x == n) acc
    else calcRow(x + 1, y, n, acc + grid(y)(x).power)
  }

  val answer2 = (for {
    y <- 1 until 300
    x <- 1 until 300
  } yield ({
    (x, y, calculateSquares(x, y, 1, List((1, grid(y)(x).power))).maxBy(_._2))
  })).maxBy(_._3._2)

  println(s"Answer day $day part 2: ${answer2._1},${answer2._2}, ${answer2._3._1 + 1}, " +
    s"largest total power: ${answer2._3._2} [${System.currentTimeMillis - start2}ms]")
