package fpinscala.chapter8

import fpinscala.chapter6._

/*
trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = {
    this.check && p.check
  }
}
*/

/*
object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG, A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rng = Simple(42)
    Gen(rng)
  }
}
*/
