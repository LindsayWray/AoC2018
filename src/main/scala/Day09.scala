import scala.io.*
object Day09 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val start1: Long = System.currentTimeMillis

  val pattern = "(\\d+) players; last marble is worth (\\d+) points".r
  val data: Data =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .mkString
    match {
      case pattern(players, marbles) => Data(players.toInt, marbles.toInt)
    }

  case class Data(players: Int, marbles: Int)
  case class Player(id: Int, score: Long)

  case class MarbleSeq(front : List[Int], back : List[Int]){
    def shiftRight(n: Int): MarbleSeq = {
      if(n == 0) this
      else back match
        case Nil => MarbleSeq(back, front.reverse).shiftRight(n)
        case (h :: t) => MarbleSeq(h :: front, t).shiftRight(n - 1)
    }

    def shiftLeft(n: Int): MarbleSeq = {
      if (n == 0) this
      else front match
        case Nil => MarbleSeq(back.reverse, front).shiftLeft(n)
        case (h :: t) => MarbleSeq(t, h :: back).shiftLeft(n - 1)
    }

    def insert(value: Int): MarbleSeq = {
      MarbleSeq(value :: front, back)
    }

    def remove: MarbleSeq = {
      MarbleSeq(back.head :: front.tail, back.tail)
    }

    def getValue: Int = {
      front.head
    }
  }

  def placeMarble(finalMarble : Int = data.marbles): List[Player] = {
    def go(marble: Int, player: Int, marbleSeq: MarbleSeq, players: List[Player]): List[Player] = {
      if(marble == finalMarble + 1) return players

      val nextPlayer : Int = if player < data.players then player + 1 else 1
      if(marble % 23 != 0){
        go(marble + 1, nextPlayer, marbleSeq.shiftRight(1).insert(marble), players)
      } else {
        val newMarbleSeq : MarbleSeq = marbleSeq.shiftLeft(7)
        val points : Int = newMarbleSeq.getValue + marble
        go(marble + 1, nextPlayer, newMarbleSeq.remove, players.prepended(Player(player, points)))
      }
    }
    go(1, 1, MarbleSeq(List(0), Nil), Nil)
  }

  val answer1 : Long = placeMarble().groupBy(p => p.id).map(e => e._2.map(_.score).sum).max
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2 : Long = placeMarble(data.marbles * 100).groupBy(p => p.id).map(e => e._2.map(_.score).sum).max
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

