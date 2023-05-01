import Day08.Node

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

  case class Node(children: List[Node], meta: Vector[Int], end: Int)
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
  def countMetaValues(node: Node): Int = {
    node.meta.sum + node.children.map(countMetaValues).sum
  }

  val answer1: Int = countMetaValues(root)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  // PART 2
  def countNodeValue(node: Node): Int = {
    if(node.children.isEmpty) node.meta.sum
    else {
      node.meta.map(m => {
        if(m <= node.children.size) countNodeValue(node.children(m - 1))
        else 0
      }).sum
    }
  }

  val answer2: Int = countNodeValue(root)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")




