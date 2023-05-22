import scala.io.*

object Day14 extends App:

  val day: String = this.getClass.getName.drop(3).init
  val digits: String = Source.fromResource(s"input$day.txt").mkString
  val input = digits.toInt

  case class Elf(current: Int) {
    def update(recipes: Vector[Int]): Elf = {
      Elf((current + recipes(current) + 1) % recipes.size)
    }
  }

  def createRecipes(elves: (Elf, Elf), recipes: Vector[Int], found: Vector[Int] => Boolean): Vector[Int] = {
    if (found(recipes)) recipes
    else {
      val newRecipe: Int = recipes(elves._1.current) + recipes(elves._2.current)
      val newRecipes: Vector[Int] = if (newRecipe > 9) then recipes.appended(1).appended(newRecipe - 10)
      else recipes.appended(newRecipe)
      createRecipes((elves._1.update(newRecipes), elves._2.update(newRecipes)), newRecipes, found)
    }
  }

  val start1: Long = System.currentTimeMillis
  val answer1: String = createRecipes(Elf(0) -> Elf(1), Vector[Int](3, 7),
    (_.size - 1 >= input + 10)).slice(input, input + 10).mkString
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  //PART 2
  val start2: Long = System.currentTimeMillis
  val score = digits.toVector.map(_.toInt - '0')

  val recipes: Vector[Int] = createRecipes(Elf(0) -> Elf(1), Vector[Int](3, 7),
    (r => r.indexOfSlice(score, r.size - 7) != -1))
  val answer2: Int = recipes.indexOfSlice(score, recipes.size - 7)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
