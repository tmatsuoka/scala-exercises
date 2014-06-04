package fpinscala.datastructures.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) { dropWhile(xs, f) } else xs
  }
  def drop[A](ls: List[A], n: Int): List[A] = {
    if (n <= 0) {
      ls
    } else {
      ls match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }
  }
  def setHead[A](h: A, ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }
  def tail[A](ls: List[A]): List[A] = drop(ls, 1)
}

