import scala.io.*

object Day13 extends App:

  val day: String = this.getClass.getName.drop(3).init

  enum Track {
    case LCURVE, RCURVE, HORIZONTAL, VERTICAL, INTERSECTION, EMPTY
  }
  import Track._

  enum Direction {
    case RIGHT, LEFT, UP, DOWN
  }
  import Direction._

  val lines: Vector[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toVector

  val trackMap: Vector[Vector[Track]] = lines.map(_.toVector.map {
    case '/' => LCURVE
    case '\\' => RCURVE
    case '+' => INTERSECTION
    case ' ' => EMPTY
    case '-' | '>' | '<' => HORIZONTAL
    case '|' | 'v' | '^' => VERTICAL
  })

  val carts: List[Cart] = lines.zipWithIndex.flatMap(s => s._1.toList.zipWithIndex.flatMap(c => {
    c._1 match
      case '>' => Some(Cart(c._2, s._2, Direction.RIGHT))
      case '<' => Some(Cart(c._2, s._2, Direction.LEFT))
      case 'v' => Some(Cart(c._2, s._2, Direction.DOWN))
      case '^' => Some(Cart(c._2, s._2, Direction.UP))
      case _ => None
  })).toList

  case class Cart(x: Int, y: Int, direction: Direction, intersections: Int = 0) extends Ordered[Cart] {
    override def compare(that: Cart): Int =
      if ((y > that.y) || (y == that.y && x > that.x)) 1 else -1

    def move: Cart = {
      val (newX, newY): (Int, Int) = direction match
        case UP => x -> (y - 1)
        case DOWN => x -> (y + 1)
        case LEFT => (x - 1) -> y
        case RIGHT => (x + 1) -> y

      val dir: Direction = trackMap(newY)(newX) match
        case LCURVE =>
          direction match
            case LEFT | RIGHT => turnLeft
            case UP | DOWN=> turnRight
        case RCURVE =>
          direction match
            case LEFT | RIGHT => turnRight
            case UP | DOWN => turnLeft
        case HORIZONTAL | VERTICAL => direction
        case INTERSECTION =>
          intersections % 3 match
            case 0 => turnLeft
            case 1 => direction
            case 2 => turnRight
        case EMPTY => throw Exception("Off Road! 🏎️")

      Cart(newX, newY, dir, if(trackMap(newY)(newX) == INTERSECTION) then intersections + 1 else intersections)
    }

    def turnLeft: Direction = {
      direction match
        case LEFT => DOWN
        case RIGHT => UP
        case UP => LEFT
        case DOWN => RIGHT
    }

    def turnRight: Direction = {
      direction match
        case LEFT => UP
        case RIGHT => DOWN
        case UP => RIGHT
        case DOWN => LEFT
    }
  }

  def moveCarts(carts: List[Cart]): (Int, Int) = {
    def go(carts: List[Cart], acc: List[Cart]): Either[(Int, Int), List[Cart]] = {
      carts match
        case Nil => Right(acc)
        case h :: t => {
          val movedCart: Cart = h.move
          if(acc.exists(c => c.x == movedCart.x && c.y == movedCart.y) || t.exists(c => c.x == movedCart.x && c.y == movedCart.y)) {
            Left(movedCart.x, movedCart.y)
          } else go(t, movedCart +: acc)
        }
    }
    
    go(carts, List.empty) match
      case Right(list) => moveCarts(list.sorted)
      case Left(crash) => crash
  }

  val start1: Long = System.currentTimeMillis
  val answer1: (Int, Int) = moveCarts(carts.sorted)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  //PART 2
  def moveCarts2(carts: List[Cart]): (Int, Int) = {
    def go(carts: List[Cart], acc: List[Cart]): Either[(Int, Int), List[Cart]] = {
      carts match
        case Nil => Right(acc)
        case h :: t =>
          val movedCart: Cart = h.move
          val isCrash: (Cart => Boolean) = c => c.x == movedCart.x && c.y == movedCart.y
          if(acc.isEmpty && t.isEmpty) Left(movedCart.x, movedCart.y)
          else if (acc.exists(isCrash)) go(t, acc.filterNot(isCrash))
          else if (t.exists(isCrash)) go(t.filterNot(isCrash), acc)
          else go(t, movedCart +: acc)
    }

    go(carts, List.empty) match
      case Right(list) => moveCarts2(list.sorted)
      case Left(crash) => crash
  }

  val start2: Long = System.currentTimeMillis
  val answer2: (Int, Int) = moveCarts2(carts.sorted)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")





