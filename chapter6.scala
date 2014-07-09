package fpinscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RandomNumbers {
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2_a) = ra(rng)
      val (b, rng2_b) = rb(rng2_a)
      (f(a, b), rng2_b)
    }
  }
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue) {
      nonNegativeInt(nextRNG)
    } else if (n < 0) {
      (n * -1, nextRNG)
    } else {
      (n, nextRNG)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  /*
  def double(rng: RNG): (Double, RNG) = {
    val (int_val, nextRNG) = nonNegativeInt(rng)
    val double_val = int_val.toDouble / Int.MaxValue.toDouble
    (double_val, nextRNG)
  }
  */

  def double: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
}

object RandomNumbersWithFlatMap {
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => rng => (f(a), rng))
  }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => rng => {
      flatMap(rb)(b => rng_b => (f(a, b), rng_b))(rng)
    })
  }
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue) {
      nonNegativeInt(nextRNG)
    } else if (n < 0) {
      (n * -1, nextRNG)
    } else {
      (n, nextRNG)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  /*
  def double(rng: RNG): (Double, RNG) = {
    val (int_val, nextRNG) = nonNegativeInt(rng)
    val double_val = int_val.toDouble / Int.MaxValue.toDouble
    (double_val, nextRNG)
  }
  */

  def double: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[A, B](g: A => State[S, B]): State[S, B] = {
    state => {
      val (a, next) = this.run(state)
      g(a)(next)
    }
  }
  def map[A, B](s: State[S, A])(f: A => B): State[S, B] = {
    flatMap(s)(a => rng => (f(a), rng))
  }
  def map2[A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(ra)(a => rng => {
      flatMap(rb)(b => rng_b => (f(a, b), rng_b))(rng)
    })
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(state => (a, state))
}
