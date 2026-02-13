package printer

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues
import parser.Parser
import parser.ParserOps.{ P0 => POParser }
import printer.PrinterOps.{ P0 => P0Printer }

class PrinterSpec extends AnyWordSpec with EitherValues {

  "A Printer Spec" should {
    "never succeed (Printer.never)" in {
      val p = Printer.never[Int]("expected number")
      val e = p.print(42, new StringBuilder()).left.value
      assert(e.expected == "expected number")
      assert(e.found == "42")
    }

    "always succeed without consuming (Printer.always)" in {
      val p   = Printer.always[String]()
      val sb  = new StringBuilder("prefix")
      val res = p.print("anything", sb)
      assert(res.value.toString == "prefix")
    }

    "print an int parser" in {
      assert(Printer.int.print(1, new StringBuilder()).value.toString() == "1")
      assert(
        Printer.int.print(-1, new StringBuilder()).value.toString() == "-1"
      )
      assert(Printer.int.print(+1, new StringBuilder()).value.toString() == "1")
    }

    "print a double parser" in {
      assert(
        Printer.double.print(1.2, new StringBuilder()).value.toString() == "1.2"
      )
      assert(
        Printer.double
          .print(-1.2, new StringBuilder())
          .value
          .toString() == "-1.2"
      )
      assert(
        Printer.double
          .print(+1.0, new StringBuilder())
          .value
          .toString() == "1.0"
      )
      assert(
        Printer.double.print(0.4, new StringBuilder()).value.toString() == "0.4"
      )
    }

    "print a char parser" in {
      assert(
        Printer.char.print('a', new StringBuilder()).value.toString() == "a"
      )
      assert(
        Printer.char.print(' ', new StringBuilder()).value.toString() == " "
      )
    }

    "print a prefix string parser" in {
      val input = "Hello"
      val print =
        Printer.prefix(input).print((), new StringBuilder()).value.toString()
      val parse = Parser.prefix(input).parse(print)._1
      assert(print == input)
      assert(parse.isRight)
    }

    "print a prefix while parser" in {
      val input              = "Blob, Esq."
      val p: Char => Boolean = _ != '\"'
      val print              =
        Printer
          .prefix(p)
          .print(Parser.prefix(p).parse(input)._1.value, new StringBuilder())
          .value
          .toString()
      val parse =
        Parser
          .prefix(p)
          .parse(
            Printer.prefix(p).print(input, new StringBuilder()).value.toString()
          )
          ._1
          .value
      assert(print == parse)
      assert(print == input)
      assert(parse == input)
    }

    "print a zipped printer" in {
      val input  = "key:abc;"
      val parser =
        Parser
          .prefix("key:")
          .take(Parser.prefix(_ != ';'))
          .skip(Parser.prefix(";"))

      val printer =
        Printer
          .prefix("key:")
          .take(Printer.prefix(_ != ';'))
          .skip(Printer.prefix(";"))

      assert(parser.parse(input)._1.value == "abc")
      assert(
        printer.print("abc", new StringBuilder()).value.toString() == input
      )
    }

    "print a literal" in {
      val parser = Parser
        .literal("Hello")
        .zip(Parser.literal("World"))
      val printer = Printer
        .literal("Hello")
        .zip(Printer.literal("World"))

      assert(
        printer
          .print(parser.parse("HelloWorld")._1.value, new StringBuilder())
          .value
          .toString() == "HelloWorld"
      )
      assert(
        parser
          .parse(
            printer
              .print(("Hello", "World"), new StringBuilder())
              .value
              .toString()
          )
          ._1
          .value == ("Hello", "World")
      )
    }

    "print one or another printer" in {
      val printer = Printer.literal("123").or(Printer.literal("abc"))
      val parser  = Parser.literal("123").or(Parser.literal("abc"))

      assert(
        printer
          .print(parser.parse("123")._1.value, new StringBuilder())
          .value
          .toString() == "123"
      )
      assert(
        printer
          .print(parser.parse("abc")._1.value, new StringBuilder())
          .value
          .toString() == "abc"
      )
      assert(
        parser
          .parse(printer.print("123", new StringBuilder()).value.toString())
          ._1
          .value == "123"
      )
      assert(
        parser
          .parse(printer.print("abc", new StringBuilder()).value.toString())
          ._1
          .value == "abc"
      )

      val error = printer.print("ABC", new StringBuilder()).left.value
      assert(error.expected == "123 or abc")
      assert(error.found == "ABC")
    }

    "print one of many printers" in {
      val printer = Printer.oneOf(
        Printer.literal("123"),
        Printer.literal("abc"),
        Printer.literal("ABC")
      )
      val parser = Parser.oneOf(
        Parser.literal("123"),
        Parser.literal("abc"),
        Parser.literal("ABC")
      )

      assert(
        printer
          .print(parser.parse("123")._1.value, new StringBuilder())
          .value
          .toString() == "123"
      )
      assert(
        printer
          .print(parser.parse("abc")._1.value, new StringBuilder())
          .value
          .toString() == "abc"
      )
      assert(
        printer
          .print(parser.parse("ABC")._1.value, new StringBuilder())
          .value
          .toString() == "ABC"
      )
      assert(
        parser
          .parse(printer.print("123", new StringBuilder()).value.toString())
          ._1
          .value == "123"
      )
      assert(
        parser
          .parse(printer.print("abc", new StringBuilder()).value.toString())
          ._1
          .value == "abc"
      )
      assert(
        parser
          .parse(printer.print("ABC", new StringBuilder()).value.toString())
          ._1
          .value == "ABC"
      )

      val error = printer.print("+-/*", new StringBuilder()).left.value
      assert(error.expected == "123 or abc or ABC")
      assert(error.found == "+-/*")
    }

    "print zero or more printer" in {
      assert(
        Printer.int
          .zeroOrMore()
          .print(List.empty, new StringBuilder())
          .value
          .isEmpty
      )
      assert(
        Printer.int
          .zeroOrMore()
          .print(List(1, 2, 3, 4), new StringBuilder())
          .value
          .toString() == "1234"
      )
    }

    "print zero or more printer with separator" in {
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(","))
          .print(List.empty, new StringBuilder())
          .value
          .isEmpty
      )
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(", "))
          .print(List(1, 2, 3, 4), new StringBuilder())
          .value
          .toString() == "1, 2, 3, 4"
      )
    }

    "print zero or more printer with separator and terminator" in {
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(","), Printer.prefix("."))
          .print(List.empty, new StringBuilder())
          .value
          .toString() == "."
      )
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(", "), Printer.newline)
          .print(List(1, 2, 3, 4), new StringBuilder())
          .value
          .toString() == "1, 2, 3, 4\n"
      )
    }

    "zeroOrMore returns element error when an element fails to print" in {
      val p = Printer
        .oneOf(Printer.literal("a"), Printer.literal("b"))
        .zeroOrMore(Printer.prefix(","), Printer.prefix("."))
      val sb  = new StringBuilder()
      val res = p.print(List("a", "x", "b"), sb)
      assert(res.isLeft)
      val e = res.left.value
      assert(e.expected == "a or b")
      assert(e.found == "x")
    }

    "contraMap and contraMapSuccess" in {
      sealed trait T
      case class A(i: Int)    extends T
      case class B(s: String) extends T
      val printerA = Printer.int.contraMapSuccess[A](a => a.i)
      val printerB = Printer.literal("foo").contraMapSuccess[B](b => b.s)
      assert(printerA.print(A(42), new StringBuilder()).value.toString == "42")
      assert(printerB.print(B("foo"), new StringBuilder()).value.toString == "foo")

      val fromEither =
        Printer.int.contraMap[Either[String, Int]](_.fold(s => Left(new RuntimeException(s)), a => Right(a)))
      assert(fromEither.print(Right(7), new StringBuilder()).value.toString == "7")
      val err = fromEither.print(Left("bad"), new StringBuilder()).left.value
      assert(err.expected.contains("bad"))
    }

    "repeat a printer several times" in {
      assert(
        Printer.whitespace
          .repeat(2)
          .print((), new StringBuilder())
          .value
          .toString() == "  "
      )
      assert(
        Printer.int
          .repeat(2)
          .print(1, new StringBuilder())
          .value
          .toString() == "11"
      )
      assert(
        Printer
          .literal("Hi")
          .repeat(2)
          .print("Hi", new StringBuilder())
          .value
          .toString() == "HiHi"
      )

      val e = Printer
        .literal("Hi")
        .repeat(2)
        .print("Hello", new StringBuilder())
        .left
        .value
      assert(e.expected == "Hi")
      assert(e.found == "Hello")
    }

    "map a printer" in {
      val role: Printer[Int] = Printer.oneOf(
        Printer
          .prefix("admin")
          .map(_ => 0),
        Printer
          .prefix("guest")
          .map(_ => 1),
        Printer
          .prefix("member")
          .map(_ => 2)
      )

      assert(role.print(0, new StringBuilder()).value.toString() == "admin")
      assert(role.print(1, new StringBuilder()).value.toString() == "guest")
      assert(role.print(2, new StringBuilder()).value.toString() == "member")

      val e = role.print(3, new StringBuilder()).left.value
      assert(e.expected == "0 or 1 or 2")
      assert(e.found == "3")
    }

    "skip a printer" in {
      assert(
        Printer.int
          .skipOpt(Some(Printer.prefix("abc")))
          .print(123, new StringBuilder())
          .value
          .toString() == "123abc"
      )
      assert(
        Printer.int
          .skipOpt(None)
          .print(123, new StringBuilder())
          .value
          .toString() == "123"
      )
    }
  }
}
