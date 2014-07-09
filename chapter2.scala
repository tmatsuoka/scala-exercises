
object FPGroup {

    //
    // Chapter 2
    //

    // Exercise 1

    def fib(n: Int): Int = {
        // n is counter
        // a is fib(n-1)
        // b is fib(n-2)
        @annotation.tailrec
        def fib_tail(n: Int, a: Int, b: Int): Int = n match {
            case 0 => b
            case _ => fib_tail(n - 1, a + b, a)
        }
        fib_tail(n, 1, 0)
    }

    def fibs(n: Int): StringBuilder = {
        val str = new StringBuilder()
        def collect(n: Int, s: Array[Int]): Array[Int] = n match {
            case 0 => s
            case _ => collect(n - 1, fib(n - 1) +: s)
        }
        collect(n, Array(fib(n))).addString(str, ", ")
    }

    // Exercise 2

    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = as.length match {
        case 0 => true
        case 1 => true
        case _ => gt(as(0), as(1)) && isSorted(as.tail, gt)
    }

    // Exercise 3: Currying
    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
        a => (b => f(a,b))

    // Exercise 4: Uncurrying
    def uncurry[A,B,C](f: A => (B => C)): (A, B) => C =
        (a, b) => f(a)(b)

    // Exercise 5: Composition
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f(g(a))

    //
    // Chapter 3
    //

    sealed trait List[+A] {

        // Exercise 2
        def tail[A](): List[A] = this match {
            case Nil => Nil // throw Error!
            case Cons(x,xs) => xs
        }

        // def tail(): List[A]

        // Exercise 3
        def setHead[A](head: A): List[A] = this match {
            case Nil => List(head)
            case _ => Cons(head, this.tail)
        }

        // Exercise 4
        def drop[A](n: Int): List[A] = {
            def loop(n: Int, xs: List[A]): List[A] = n match {
                case 0 => xs
                case _ => loop(n - 1, xs.tail)
            }
            loop(n, this)
        }
    }
    case object Nil extends List[Nothing] {

        // def tail(): List[Nothing] = Nil

    }
    case class Cons[+A](h: A, t: List[A]) extends List[A] {

        // def tail(): List[A] = t
    }

    object List {
        def sum(ints: List[Int]): Int = ints match {
            case Nil => 0
            case Cons(x,xs) => x + sum(xs)
        }

        def product(ds: List[Double]): Double = ds match {
            case Nil => 1.0
            case Cons(0.0, _) => 0.0
            case Cons(x,xs) => x * product(xs)
        }

        def apply[A](as: A*): List[A] =
            if (as.isEmpty) Nil
            else Cons(as.head, apply(as.tail: _*))

    // Exercise 1
        val x3_1 = List(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + List.sum(t)
            case _ => 101
        }

    }


    // Main

    def main(args: Array[String]): Unit = {
        println(fibs(10).toString())
        println(isSorted((1 to 10).toArray ++ Array(2), ((x: Int,y: Int) => x < y)))

        val l = List(1,2,3,4,5,6,7,8,9,10)

        println(List.tail(l))
        println(List.setHead(l, 11))
    }
}

