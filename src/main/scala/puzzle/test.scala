package puzzle

/**
 * Created by Andrei Ilchenko  on 7-2-16.
 */
object test extends App {
  val problem = new Combi(List(1, 3, 4, 6))
  problem.solutions(24).foreach(x => println(x))
}
