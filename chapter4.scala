package fpinscala.datastructures.chapter4

object Mean {
    def mean(ints: Seq[Double]): Option[Double] = {
        if (ints.isEmpty) {
            None
        } else {
            Some(ints.sum / ints.length)
        }
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None    => None
        case Some(v) => Some(f(v))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None    => None
        case Some(v) => f(v)
    }
    def getOrElse[B >: A](default: => B): B = this match {
        case None    => default
        case Some(v) => v
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None    => ob
        case Some(_) => this
    }
    def filter(f: A => Boolean): Option[A] = this match {
        case None    => None
        case Some(v) => if (f(v)) { Some(v) } else None
    }
    def variance(xs: Seq[Double]): Option[Double] = {
        Mean.mean(xs).flatMap({ m => Mean.mean(xs.map({ x => math.pow(x - m, 2) })) })
    }
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a.flatMap({ a_val => b.flatMap({ b_val => Some(f(a_val, b_val)) }) })
    }
}

object Option {
    def sequence_nestmatch[A](a: List[Option[A]]): Option[List[A]] = {
        a match {
            case head :: tail => {
                head match {
                    case Some(head_val) => {
                        sequence_nestmatch(tail) match {
                            case Some(tail_vals) => Some(head_val :: tail_vals)
                            case None            => None
                        }
                    }
                    case None => None
                }
            }
            case Nil          => Some(Nil)
        }
    }
    def sequence_flatmap[A](a: List[Option[A]]): Option[List[A]] = {
        a match {
            case head :: tail => head.flatMap({ head_val => sequence_flatmap(tail).flatMap({ tail_vals => Some(head_val :: tail_vals) }) })
            case Nil          => Some(Nil)
        }
    }
    def sequence_foryield[A](a: List[Option[A]]): Option[List[A]] = {
        a match {
            case head :: tail => {
                for {
                    head_val <- head
                    tail_vals <- sequence_foryield(tail)
                } yield head_val :: tail_vals
            }
            case Nil          => Some(Nil)
        }
    }
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
        a match {
            // case head :: tail => f(head).flatMap({ x => traverse(tail)(f).flatMap({ xs => Some(x :: xs) }) })
            case head :: tail => {
                for {
                    x <- f(head)
                    xs <- traverse(tail)(f)
                } yield x :: xs
            }
            case Nil          => Some(Nil)
        }
    }
    def sequence_traverse[A](a: List[Option[A]]): Option[List[A]] = {
        traverse(a)({ x => x })
    }
}

sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Right(v) => Right(f(v))
        case Left(e)  => Left(e) // 'this' is Either[E, A] but return value is Either[E, B] so value needs to be recreated
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Right(v) => f(v)
        case Left(e)  => Left(e) // 'this' is Either[E, A] but return value is Either[E, B] so value needs to be recreated
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Right(_) => this
        case Left(_)  => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
        for {
            a_val <- this
            b_val <- b
        } yield f(a_val, b_val)
    }
}

object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
        traverse(es)({ e => e })
    }
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
        as match {
            case head :: tail => {
                for {
                    x <- f(head)
                    xs <- traverse(tail)(f)
                } yield x :: xs
            }
            case Nil          => Right(Nil)
        }
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Main extends App {
    val foo1 = List(Some(5), Some(2), Some(10))
    println(Option.sequence_nestmatch(foo1))
    println(Option.sequence_flatmap(foo1))
    println(Option.sequence_foryield(foo1))
    println(Option.sequence_traverse(foo1))
    val foo2 = List(Some(11), None, Some(3), Some(4))
    println(Option.sequence_nestmatch(foo2))
    println(Option.sequence_flatmap(foo2))
    println(Option.sequence_foryield(foo2))
    println(Option.sequence_traverse(foo2))
    val foo3 = List()
    println(Option.sequence_nestmatch(foo3))
    println(Option.sequence_flatmap(foo3))
    println(Option.sequence_foryield(foo3))
    println(Option.sequence_traverse(foo3))
    val foo4 = List(2, 4, 6, 7)
    println(Option.traverse(foo4)({ x => if (x % 2 == 0) { Some(x * 2) } else { None } }))

    println("-----------")
    val bar1 = List(Right(5), Right(2), Right(10))
    println(Either.sequence(bar1))
    val bar2 = List(Right(11), Left("Error Occurred"), Right(3), Right(4))
    println(Either.sequence(bar2))
    val bar3 = List()
    println(Either.sequence(bar3))
    val bar4 = List(2, 4, 6, 7)
    println(Either.traverse(foo4)({ x => if (x % 2 == 0) { Right(x * 2) } else { Left(x + " is not even") } }))
}
