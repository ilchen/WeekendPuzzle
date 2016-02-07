package streams

/**
 * Created by ilchen on 7-2-16.
 */
object test extends App {
  val problem = new Combi(Vector(1, 3, 4, 6))
  problem.solutions(24).foreach(x => println(x))
  problem.exprsSets.foreach(_.foreach(x => println(x)))

  val problem2 = new Combi2(List(1, 3, 4, 6))
  problem2.solutions(24).foreach(x => println(x))
}
