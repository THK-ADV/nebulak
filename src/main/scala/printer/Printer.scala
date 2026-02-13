package printer

import Printer.always

/** Prints a value of type `A` into a `StringBuilder`; returns the builder on success or an error. */
case class Printer[A](print: (A, StringBuilder) => Either[PrintingError, StringBuilder]) extends AnyVal {

  /** Adapts this printer to type `B` by converting with `f`; fails if `f` returns Left. */
  inline def contraMap[B](f: B => Either[Throwable, A]): Printer[B] = Printer {
    case (b, input) =>
      f(b) match {
        case Right(a) => print(a, input)
        case Left(e)  => Left(PrintingError(e.getMessage, input.toString()))
      }
  }

  /** Like `contraMap` but with a total function (no failure from conversion). */
  inline def contraMapSuccess[B](f: B => A): Printer[B] = contraMap(b => Right(f(b)))

  /** Prints a pair: runs this on the first element, then `that` on the second into the same builder. */
  inline def zip[B](that: Printer[B]): Printer[(A, B)] = Printer {
    case ((a, b), input) =>
      this.print(a, input).flatMap(that.print(b, _))
  }

  /** Same as `zip(that)` but only the value is passed; the unit printer gets `()`. */
  inline def skip(that: Printer[Unit]): Printer[A] =
    this.zip(that).contraMapSuccess(a => (a, ()))

  /** If `that` is defined, same as `skip(that)`; otherwise this printer unchanged. */
  inline def skipOpt(that: Option[Printer[Unit]]): Printer[A] =
    that.fold(this)(that => this.skip(that))

  inline def or(that: => Printer[A]): Printer[A] = Printer {
    case (a, input) =>
      this.print(a, input) match {
        case Right(newInput) => Right(newInput)
        case Left(e1)        =>
          that.print(a, input) match {
            case Right(newInput) => Right(newInput)
            case Left(e2)        =>
              Left(PrintingError(s"${e1.expected} or ${e2.expected}", a.toString))
          }
      }
  }

  /** Prints each element with optional separator between and terminator after. */
  def zeroOrMore(
      separator: Printer[Unit] = always(),
      terminator: Printer[Unit] = always()
  ): Printer[List[A]] = Printer {
    case (as, input) =>
      var firstElement = true
      var soFar        = input
      var loopError    = Option.empty[PrintingError]
      var index        = 0

      while (loopError.isEmpty && index < as.size) {
        val a = as(index)
        if (!firstElement) {
          separator.print((), soFar) match {
            case Right(soFar0) => soFar = soFar0
            case Left(e)       => loopError = Some(e)
          }
        }
        this.print(a, soFar) match {
          case Right(soFar0) => soFar = soFar0
          case Left(e)       => loopError = Some(e)
        }
        firstElement = false
        index += 1
      }

      loopError match {
        case Some(e) => Left(e)
        case None    =>
          terminator.print((), soFar) match {
            case Right(soFar0) => Right(soFar0)
            case Left(e)       =>
              Left(PrintingError(s"terminator: ${e.expected}", soFar.toString()))
          }
      }
  }

  /** Prints the same value `times` in a row. */
  def repeat(times: Int): Printer[A] = Printer {
    case (a, input) =>
      var count = 0
      var error = Option.empty[PrintingError]
      var soFar = input

      while (error.isEmpty && count < times) {
        this.print(a, soFar) match {
          case Right(input0) => soFar = input0
          case Left(e)       => error = Some(e)
        }
        count += 1
      }

      error.toLeft(soFar)
  }
}

object Printer {

  /** Appends the integer to the builder. */
  val int: Printer[Int] = Printer {
    case (int, input) =>
      Right(input.append(int))
  }

  /** Appends the double to the builder. */
  val double: Printer[Double] = Printer {
    case (double, input) =>
      Right(input.append(double))
  }

  /** Appends the character to the builder. */
  val char: Printer[Char] = Printer {
    case (char, input) =>
      Right(input.append(char))
  }

  /** Appends a single space. */
  val whitespace: Printer[Unit] = Printer {
    case (_, input) =>
      Right(input.append(' '))
  }

  /** Appends a newline. */
  val newline: Printer[Unit] = Printer {
    case (_, input) =>
      Right(input.append('\n'))
  }

  /** Always fails with the given expected message. */
  def never[A](expected: String = "fail"): Printer[A] = Printer {
    case (a, _) =>
      Left(PrintingError(expected, a.toString))
  }

  /** Succeeds without appending anything. */
  def always[A](): Printer[A] = Printer {
    case (_, input) =>
      Right(input)
  }

  /** Appends the fixed string `p`. */
  def prefix(p: String): Printer[Unit] = Printer {
    case (_, input) =>
      Right(input.append(p))
  }

  /** Prints the value only if it starts with `lit` (appends `lit`). */
  def literal(lit: String): Printer[String] = Printer {
    case (output, input) =>
      if (output.startsWith(lit)) Right(input.append(lit))
      else Left(PrintingError(lit, output))
  }

  /** Appends the value only if every character satisfies `p`. */
  def prefix(p: Char => Boolean): Printer[String] = Printer {
    case (output, input) =>
      if (output.forall(p)) Right(input.append(output))
      else Left(PrintingError(s"$output to match the prefix", output))
  }

  /** Tries each printer in order; succeeds with the first success. */
  def oneOf[A](ps: Printer[A]*): Printer[A] = ps.size match {
    case 0 => never("one printer")
    case 1 => ps.head
    case _ => ps.reduce(_ or _)
  }

  def oneOf[A](ps: List[Printer[A]]): Printer[A] = oneOf(ps: _*)
}
