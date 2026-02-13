package parser

import scala.collection.mutable.ListBuffer

/** Parses a prefix of a string into a value of type `A`, returning the result and the remaining string. */
case class Parser[A](parse: String => (Either[ParsingError, A], String)) extends AnyVal {
  import Parser.always
  import Parser.never

  /** Transforms the result on success; leaves input and error unchanged on failure. */
  inline def map[B](f: A => B): Parser[B] = Parser { str =>
    val (a, restA) = parse(str)
    a match {
      case Right(a) => Right(f(a)) -> restA
      case Left(e)  => Left(e)     -> str
    }
  }

  /** Runs this parser, then the parser returned by `f` on the rest; fails if either fails. */
  inline def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { str =>
    val (a, restA) = parse(str)
    a match {
      case Right(a) =>
        val (b, restB) = f(a).parse(restA)
        b match {
          case Right(b) => Right(b) -> restB
          case Left(e)  => Left(e)  -> str
        }
      case Left(e) => Left(e) -> str
    }
  }

  /** Tries this parser; if it fails, tries `that` on the same input. */
  inline def or[B >: A](that: => Parser[B]): Parser[B] = Parser { str =>
    val (a, restA) = this.parse(str)
    a match {
      case Right(a) => Right(a) -> restA
      case Left(e1) =>
        val (b, restB) = that.parse(str)
        b match {
          case Right(b) => Right(b) -> restB
          case Left(e2) =>
            val e = ParsingError(s"${e1.expected} or ${e2.expected}", e2.found)
            Left(e) -> str
        }
    }
  }

  /** Runs this then `that` on the rest; succeeds with the pair of results. */
  inline def zip[B](that: Parser[B]): Parser[(A, B)] = Parser { str =>
    val (a, restA) = this.parse(str)
    a match {
      case Right(a) =>
        val (b, restB) = that.parse(restA)
        b match {
          case Right(b) => Right((a, b)) -> restB
          case Left(e)  => Left(e)       -> str
        }
      case Left(e) => Left(e) -> str
    }
  }

  /** Same as `zip(that)` but keeps only this parser's result. */
  inline def skip[B](that: Parser[B]): Parser[A] = this.zip(that).map(_._1)

  /** Repeatedly parses this (with optional separator/terminator) into a list; enforces `minimum` elements. */
  def many(
      separator: Parser[Unit] = always(()),
      terminator: Parser[Unit] = always(()),
      minimum: Int = 0
  ): Parser[List[A]] =
    if (minimum < 0) never("minimum to be positive")
    else
      Parser { str =>
        val as              = ListBuffer[A]()
        var soFar           = str
        var loopError       = Option.empty[ParsingError]
        var terminatorError = Option.empty[ParsingError]
        var firstElement    = true

        while (loopError.isEmpty) {
          if (!firstElement) {
            val (sep, rest2) = separator.parse(soFar)
            sep match {
              case Right(_) =>
                soFar = rest2
              case Left(e) =>
                loopError = Some(
                  ParsingError(s"separator: ${e.expected}", e.found)
                )
            }
          }
          val (a, rest) = parse(soFar)
          a match {
            case Right(a) =>
              as += a
              soFar = rest
            case Left(e) =>
              loopError = Some(e)
          }
          firstElement = false
        }

        val (term, rest) = terminator.parse(soFar)
        term match {
          case Right(_) =>
            soFar = rest
          case Left(e) =>
            terminatorError = Some(
              ParsingError(s"terminator: ${e.expected}", e.found)
            )
        }

        terminatorError match {
          case None if as.size >= minimum =>
            Right(as.toList) -> soFar
          case Some(e) =>
            Left(e) -> str
          case _ =>
            Left(ParsingError(s"minimum of $minimum elements", soFar)) -> str
        }
      }

  /** Parses this as long as input is non-empty and parsing succeeds; separator between elements. */
  def all(separator: Parser[Unit] = always(())): Parser[List[A]] =
    Parser { str =>
      val as           = ListBuffer[A]()
      var soFar        = str
      var loopError    = Option.empty[ParsingError]
      var firstElement = true

      while (loopError.isEmpty && soFar.nonEmpty) {
        if (!firstElement) {
          val (sep, rest2) = separator.parse(soFar)
          sep match {
            case Right(_) =>
              soFar = rest2
            case Left(e) =>
              loopError = Some(
                ParsingError(s"separator: ${e.expected}", e.found)
              )
          }
        }
        if (loopError.isEmpty) {
          val (a, rest) = parse(soFar)
          a match {
            case Right(a) =>
              as += a
              soFar = rest
            case Left(e) =>
              loopError = Some(e)
          }
          firstElement = false
        }
      }

      loopError match {
        case Some(e) =>
          Left(e) -> str
        case None =>
          Right(as.toList) -> soFar
      }
    }

  /** Wraps the result in `Some` on success, or returns `None` without consuming input on failure. */
  inline def option: Parser[Option[A]] = Parser { str =>
    val (res, rest) = this.parse(str)
    res match {
      case Right(a) => Right(Some(a)) -> rest
      case Left(_)  => Right(None)    -> str
    }
  }
}

object Parser {

  /** Succeeds immediately with `a`, consuming no input. */
  def always[A](a: A): Parser[A] =
    Parser(str => Right(a) -> str)

  /** Always fails with the given expected message; consumes no input. */
  def never[A](expected: String = "fail"): Parser[A] =
    Parser(str => Left(ParsingError(expected, str)) -> str)

  /** Parses a single character. */
  val char: Parser[Char] = Parser { str =>
    if (str.isEmpty) Left(ParsingError("a char", str)) -> str
    else Right(str.head)                               -> str.tail
  }

  /** Parses optional '+' or '-' and returns 1 or -1. */
  val sign: Parser[Int] = Parser { str =>
    str.headOption match {
      case Some(plus) if plus == '+' =>
        Right(1) -> str.tail
      case Some(minus) if minus == '-' =>
        Right(-1) -> str.tail
      case _ =>
        Left(ParsingError("+ or -", str)) -> str
    }
  }

  /** Parses an optional sign followed by digits into an integer. */
  val int: Parser[Int] = Parser { str =>
    val (sign, str0) = this.sign.or(always(1)).parse(str)
    sign match {
      case Right(s) =>
        val prefix = str0.takeWhile(_.isDigit)
        prefix.toIntOption match {
          case Some(i) => Right(i * s)                          -> str0.stripPrefix(prefix)
          case None    => Left(ParsingError("an integer", str)) -> str
        }
      case Left(e) =>
        Left(e) -> str
    }
  }

  /** Parses an optional sign followed by a decimal number. */
  val double: Parser[Double] = Parser { str =>
    val (sign, str0) = this.sign.or(always(1)).parse(str)
    sign match {
      case Right(s) =>
        var decimalCount = 0
        val prefix       = str0.takeWhile { c =>
          if (c == '.') decimalCount += 1
          c.isDigit || (c == '.' && decimalCount <= 1)
        }
        prefix.toDoubleOption match {
          case Some(d) => Right(d * s)                        -> str0.stripPrefix(prefix)
          case None    => Left(ParsingError("a double", str)) -> str
        }
      case Left(e) =>
        Left(e) -> str
    }
  }

  /** Parses "true" or "false". */
  val boolean: Parser[Boolean] =
    oneOf(
      prefix("true").map(_ => true),
      prefix("false").map(_ => false)
    )

  /** Parses a single whitespace character. */
  val whitespace: Parser[Unit] = Parser { str =>
    if (str.headOption.exists(_.isWhitespace)) Right(()) -> str.tail
    else Left(ParsingError("whitespace", str))           -> str
  }

  /** Consumes zero or more whitespace characters; always succeeds. */
  val zeroOrMoreSpaces: Parser[Unit] = Parser { str =>
    Right(()) -> str.dropWhile(_.isWhitespace)
  }

  /** Parses a newline (\\n or \\r\\n). */
  val newline: Parser[Unit] = Parser { str =>
    if (str.startsWith("\n"))
      Right(()) -> str.tail
    else if (str.startsWith("\r\n"))
      Right(()) -> str.drop(2)
    else
      Left(ParsingError("newline", str)) -> str
  }

  /** Succeeds only when the input is empty. */
  val end: Parser[Unit] = Parser { str =>
    Either.cond(str.isEmpty, (), ParsingError("end of input", str)) -> str
  }

  /** Consumes and returns the entire remaining input. */
  val rest: Parser[String] = Parser(str => Right(str) -> "")

  /** Succeeds if the input starts with `p`, consuming it. */
  def prefix(p: String): Parser[Unit] = Parser { str =>
    if (str.startsWith(p)) Right(()) -> str.drop(p.length)
    else Left(ParsingError(p, str))  -> str
  }

  /** Like `prefix(lit)` but returns the matched string. */
  def literal(lit: String): Parser[String] = prefix(lit).map(_ => lit)

  /** Parses the longest prefix of characters satisfying `p` and returns it. */
  def prefix(p: Char => Boolean): Parser[String] = Parser { str =>
    val prefix = str.takeWhile(p)
    if (prefix.isEmpty) Left(ParsingError("a given prefix", str)) -> str
    else Right(prefix)                                            -> str.stripPrefix(prefix)
  }

  /** Parses and returns the substring before the first `string`; consumes the delimiter. */
  def prefixTo(string: String): Parser[String] = Parser { str =>
    val end = str.indexOf(string)
    if (end != -1) {
      val s = str.slice(0, end)
      Right(s) -> str.drop(end + string.length)
    } else {
      Left(ParsingError(string, str)) -> str
    }
  }

  /** Parses and returns the substring before the first `string`; leaves the delimiter in the rest. */
  def prefixUntil(string: String): Parser[String] = Parser { str =>
    val end = str.indexOf(string)
    if (end != -1) {
      val s = str.slice(0, end)
      Right(s) -> str.drop(end - 1 + string.length)
    } else {
      Left(ParsingError(string, str)) -> str
    }
  }

  /** Tries each parser in order; succeeds with the first success. */
  def oneOf[A](ps: Parser[A]*): Parser[A] = ps.size match {
    case 0 => never("one parser")
    case 1 => ps.head
    case _ => ps.reduce(_ or _)
  }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] = oneOf(ps: _*)

  /** Runs `p` and discards its result (success with `()`). */
  def skipFirst[A](p: Parser[A]): Parser[Unit] = p.map(_ => ())

  /** Runs `p` or succeeds with `()` without consuming input. */
  def optional[A >: Unit](p: Parser[A]): Parser[A] = p.or(always(()))

  /** Succeeds when `p` fails; fails when `p` succeeds; consumes no input. */
  def not[A >: Unit](p: Parser[A]): Parser[A] = Parser { str =>
    val (res, _) = p.parse(str)
    res match {
      case Right(_) => Left(ParsingError("not to succeed", str)) -> str
      case Left(_)  => Right(())                                 -> str
    }
  }

  /** Parses from the start up to (but not including) the first `to`; input must start with `from`. */
  def range(from: String, to: String): Parser[String] =
    Parser { str =>
      if (!str.startsWith(from))
        Left(
          ParsingError(s"string to start with $from as a lower bound", str)
        ) -> str
      else {
        val end = str.indexOf(to)
        if (end == -1)
          Left(
            ParsingError(s"string to contain $to as a upper bound", str)
          ) -> str
        else {
          val res = str.slice(0, end)
          Right(res) -> str.drop(end)
        }
      }
    }
}
