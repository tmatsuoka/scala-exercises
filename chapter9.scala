object Parsing {

  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    // Primitives
    implicit def string(s: String): Parser[String]
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
    def slice[A](p: Parser[A]): Parser[String]
    def succeed[A](a: A): Parser[A]
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    // Combinators
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
      flatMap(a)(v => succeed(f(v)))
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C]

    def char(c: Char): Parser[Char] =
      map(string(c.toString))(_.charAt(0))
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
      ParserOps[String] = ParserOps(f(a))
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    // def many[A](p: Parser[A]): Parser[List[A]]
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())
    // def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    //   map(product(p, p2))(f.tupled)
    // def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      map2(p, p2)((a, b) => (a, b))
    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def product[B >: A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    }
  }

}
