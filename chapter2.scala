package fpinscala.chapter2

object Chapter2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(m: Int, prev_1: Int, prev_2: Int): Int = {
      val newval = prev_1 + prev_2
      if (m < n) {
        loop(m + 1, newval, prev_1)
      } else {
        newval
      }
    }
    if ((n == 0) || (n == 1)) {
      n
    } else {
      loop(2, 0, 1)
    }
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) {
        true
      } else if (gt(as(n - 1), as(n))) {
        loop(n + 1)
      } else {
        false
      }
    }
    loop(1)
  }
}

object Test {
  def run() = {
    println(Chapter2.fib(10))
    println(Chapter2.isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x <= y))
    println(Chapter2.isSorted(Array(4, 3, 2, 1), (x: Int, y: Int) => x <= y))
  }
}
