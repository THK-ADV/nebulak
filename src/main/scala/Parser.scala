import Parser.always

import scala.collection.mutable.ListBuffer

case class Parser[A](run: String => (Either[ParsingError, A], String)) {
  def map[B](f: A => B): Parser[B] = Parser { str =>
    val (a, restA) = run(str)
    a match {
      case Right(a) => Right(f(a)) -> restA
      case Left(e)  => Left(ParsingError(e.expected, str)) -> str
    }
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { str =>
    val (a, restA) = run(str)
    a match {
      case Right(a) =>
        val (b, restB) = f(a).run(restA)
        b match {
          case Right(b) => Right(b) -> restB
          case Left(e)  => Left(ParsingError(e.expected, str)) -> str
        }
      case Left(e) => Left(ParsingError(e.expected, str)) -> str
    }
  }

  def or[B >: A](p2: => Parser[B]): Parser[B] = Parser { str =>
    val (a, restA) = run(str)
    a match {
      case Right(a) => Right(a) -> restA
      case Left(e1) =>
        val (b, restB) = p2.run(str)
        b match {
          case Right(b) => Right(b) -> restB
          case Left(e2) =>
            val e = ParsingError(s"${e1.expected} or ${e2.expected}", str)
            Left(e) -> str
        }
    }
  }

  def zip[B](p2: Parser[B]): Parser[(A, B)] = Parser { str =>
    val (a, restA) = run(str)
    a match {
      case Right(a) =>
        val (b, restB) = p2.run(restA)
        b match {
          case Right(b) => Right((a, b)) -> restB
          case Left(e)  => Left(ParsingError(e.expected, str)) -> str
        }
      case Left(e) => Left(ParsingError(e.expected, str)) -> str
    }
  }

  def skip[B](p2: Parser[B]): Parser[A] = zip(p2).map(_._1)

  def zeroOrMore(
      separator: Parser[Unit] = always(()),
      terminator: Parser[Unit] = always(())
  ): Parser[List[A]] = Parser { str =>
    val as = ListBuffer[A]()
    var soFar = str
    var loopError = Option.empty[ParsingError]
    var terminatorError = Option.empty[ParsingError]

    while (loopError.isEmpty) {
      val (a, rest) = run(soFar)
      a match {
        case Right(a) =>
          as += a
          soFar = rest

          val (sep, rest2) = separator.run(soFar)
          sep match {
            case Right(_) =>
              soFar = rest2
            case Left(e) =>
              loopError = Some(
                ParsingError(s"separator: ${e.expected}", soFar)
              )
          }
        case Left(e) =>
          loopError = Some(e)
      }
    }

    val (term, rest) = terminator.run(soFar)
    term match {
      case Right(_) =>
        soFar = rest
      case Left(e) =>
        terminatorError = Some(
          ParsingError(s"terminator: ${e.expected}", soFar)
        )
    }

    terminatorError match {
      case Some(e) =>
        Left(e) -> str
      case None =>
        Right(as.toList) -> soFar
    }
  }
}

object Parser {

  def always[A](a: A): Parser[A] =
    Parser(str => Right(a) -> str)

  def never[A](expected: String = "fail"): Parser[A] =
    Parser(str => Left(ParsingError(expected, str)) -> str)

  val char: Parser[Char] = Parser { str =>
    if (str.isEmpty) Left(ParsingError("a char", str)) -> str
    else Right(str.head) -> str.tail
  }

  val sign: Parser[Int] = char.flatMap {
    case '+' => always(1)
    case '-' => always(-1)
    case _   => never("+ or -")
  }

  val int: Parser[Int] = Parser { str =>
    val (sign, str0) = (this.sign or always(1)).run(str)
    sign match {
      case Right(s) =>
        val prefix = str0.takeWhile(_.isDigit)
        prefix.toIntOption match {
          case Some(i) => Right(i * s) -> str0.stripPrefix(prefix)
          case None    => Left(ParsingError("an integer", str)) -> str
        }
      case Left(e) =>
        Left(e) -> str
    }
  }

  val double: Parser[Double] = Parser { str =>
    val (sign, str0) = (this.sign or always(1)).run(str)
    sign match {
      case Right(s) =>
        var decimalCount = 0
        val prefix = str0.takeWhile { c =>
          if (c == '.') decimalCount += 1
          c.isDigit || (c == '.' && decimalCount <= 1)
        }
        prefix.toDoubleOption match {
          case Some(d) => Right(d * s) -> str0.stripPrefix(prefix)
          case None    => Left(ParsingError("a double", str)) -> str
        }
      case Left(e) =>
        Left(e) -> str
    }
  }

  val whitespace: Parser[Unit] = char
    .flatMap(c => if (c.isWhitespace) always(()) else never("whitespace"))

  val zeroOrMoreSpaces: Parser[Unit] = Parser { str =>
    Right(()) -> str.dropWhile(_.isWhitespace)
  }

  val newline: Parser[Unit] = Parser { str =>
    if (str.startsWith("\n"))
      Right(()) -> str.tail
    else if (str.startsWith("\r\n"))
      Right(()) -> str.drop(2)
    else
      Left(ParsingError("newline", str)) -> str
  }

  val end: Parser[Unit] = Parser { str =>
    Either.cond(str.isEmpty, (), ParsingError("end of input", str)) -> str
  }

  val rest: Parser[String] = Parser(str => Right(str) -> "")

  def prefix(p: String): Parser[Unit] = Parser { str =>
    if (str.startsWith(p)) Right(()) -> str.drop(p.length)
    else Left(ParsingError(p, str)) -> str
  }

  def literal(lit: String): Parser[String] = prefix(lit).map(_ => lit)

  def prefix(p: Char => Boolean): Parser[String] = Parser { str =>
    val prefix = str.takeWhile(p)
    if (prefix.isEmpty) Left(ParsingError("a given prefix", str)) -> str
    else Right(prefix) -> str.stripPrefix(prefix)
  }

  def prefixTo(string: String): Parser[String] = Parser { str =>
    val end = str.indexOf(string)
    if (end != -1) {
      val s = str.slice(0, end)
      Right(s) -> str.drop(end + string.length)
    } else {
      Left(ParsingError(string, str)) -> str
    }
  }

  def prefixUntil(string: String): Parser[String] = Parser { str =>
    val end = str.indexOf(string)
    if (end != -1) {
      val s = str.slice(0, end)
      Right(s) -> str.drop(end - 1 + string.length)
    } else {
      Left(ParsingError(string, str)) -> str
    }
  }

  def oneOf[A](ps: Parser[A]*): Parser[A] = ps.size match {
    case 0 => never("one parser")
    case 1 => ps.head
    case _ => ps.reduce(_ or _)
  }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] = oneOf(ps: _*)

  def skipFirst[A](p: Parser[A]): Parser[Unit] = p.map(_ => ())

  def optional[A >: Unit](p: Parser[A]): Parser[A] = p or always(())

  def not[A >: Unit](p: Parser[A]): Parser[A] = Parser { str =>
    val (res, _) = p.run(str)
    res match {
      case Right(_) => Left(ParsingError("not to succeed", str)) -> str
      case Left(_)  => Right(()) -> str
    }
  }
}
