import scala.io.*

object Day08 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  private val input: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines()
      .mkString
      .split(" ")
      .map(s => s.toInt)
      .toVector

  case class Node(children: List[Node], meta: Vector[Int], end: Int) {
    def countMetaValues: Int = {
      meta.sum + children.map(_.countMetaValues).sum
    }

    // PART 2
    def countNodeValue: Int = {
      if (children.isEmpty) meta.sum
      else {
        meta.map(m => {
          if (m <= children.size) children(m - 1).countNodeValue
          else 0
        }).sum
      }
    }
  }

  def getNode(idx: Int): Node = {
    val children_amount = input(idx)
    val values = input(idx + 1)

    val children = getNodes(children_amount, idx + 2)
    val meta_start =
      if (children_amount > 0 ) children.last.end + 1
      else idx + 2

    Node(children, input.slice(meta_start, meta_start + values), meta_start + values - 1)
  }

  def getNodes(children: Int, idx: Int): List[Node] = {
    def go(lst: List[Node], count: Int, idx: Int): List[Node] = {
      if(count == 0) lst
      else {
        val n = getNode(idx)
        go(lst :+ n, count - 1, n.end + 1)
      }
    }
    go(Nil, children, idx)
  }

  val root = getNode(0)

  val answer1: Int = root.countMetaValues
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = root.countNodeValue
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
