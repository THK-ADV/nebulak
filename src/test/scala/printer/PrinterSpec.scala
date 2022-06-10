package printer

import org.scalatest.EitherValues
import org.scalatest.wordspec.AnyWordSpec
import parser.Parser
import parser.ParserOps.{P0 => POParser}
import printer.PrinterOps.{P0 => P0Printer}

class PrinterSpec extends AnyWordSpec with EitherValues {

  "A Printer Spec" should {
    "print an int parser" in {
      assert(Printer.int.print(1, "").value == "1")
      assert(Printer.int.print(-1, "").value == "-1")
      assert(Printer.int.print(+1, "").value == "1")
    }

    "print a double parser" in {
      assert(Printer.double.print(1.2, "").value == "1.2")
      assert(Printer.double.print(-1.2, "").value == "-1.2")
      assert(Printer.double.print(+1.0, "").value == "1.0")
      assert(Printer.double.print(0.4, "").value == "0.4")
    }

    "print a char parser" in {
      assert(Printer.char.print('a', "").value == "a")
      assert(Printer.char.print(' ', "").value == " ")
    }

    "print a prefix string parser" in {
      val input = "Hello"
      val print = Printer.prefix(input).print((), "").value
      val parse = Parser.prefix(input).parse(print)._1
      assert(print == input)
      assert(parse.isRight)
    }

    "print a prefix while parser" in {
      val input = "Blob, Esq."
      val p: Char => Boolean = _ != '\"'
      val print =
        Printer
          .prefix(p)
          .print(Parser.prefix(p).parse(input)._1.value, "")
          .value
      val parse =
        Parser
          .prefix(p)
          .parse(Printer.prefix(p).print(input, "").value)
          ._1
          .value
      assert(print == parse)
      assert(print == input)
      assert(parse == input)
    }

    "print a zipped printer" in {
      val input = "key:abc;"
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
      assert(printer.print("abc", "").value == input)
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
          .print(parser.parse("HelloWorld")._1.value, "")
          .value == "HelloWorld"
      )
      assert(
        parser
          .parse(printer.print(("Hello", "World"), "").value)
          ._1
          .value == ("Hello", "World")
      )
    }

    "print one or another printer" in {
      val printer = Printer.literal("123") or Printer.literal("abc")
      val parser = Parser.literal("123") or Parser.literal("abc")

      assert(printer.print(parser.parse("123")._1.value, "").value == "123")
      assert(printer.print(parser.parse("abc")._1.value, "").value == "abc")
      assert(parser.parse(printer.print("123", "").value)._1.value == "123")
      assert(parser.parse(printer.print("abc", "").value)._1.value == "abc")

      val error = printer.print("ABC", "").left.value
      assert(error.expected == "123 or abc")
      assert(error.actual == "ABC")
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

      assert(printer.print(parser.parse("123")._1.value, "").value == "123")
      assert(printer.print(parser.parse("abc")._1.value, "").value == "abc")
      assert(printer.print(parser.parse("ABC")._1.value, "").value == "ABC")
      assert(parser.parse(printer.print("123", "").value)._1.value == "123")
      assert(parser.parse(printer.print("abc", "").value)._1.value == "abc")
      assert(parser.parse(printer.print("ABC", "").value)._1.value == "ABC")

      val error = printer.print("+-/*", "").left.value
      assert(error.expected == "123 or abc or ABC")
      assert(error.actual == "+-/*")
    }

    "print zero or more printer" in {
      assert(
        Printer.int
          .zeroOrMore()
          .print(List.empty, "")
          .value == ""
      )
      assert(
        Printer.int
          .zeroOrMore()
          .print(List(1, 2, 3, 4), "")
          .value == "1234"
      )
    }

    "print zero or more printer with separator" in {
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(","))
          .print(List.empty, "")
          .value == ""
      )
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(", "))
          .print(List(1, 2, 3, 4), "")
          .value == "1, 2, 3, 4"
      )
    }

    "print zero or more printer with separator and terminator" in {
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(","), Printer.prefix("."))
          .print(List.empty, "")
          .value == "."
      )
      assert(
        Printer.int
          .zeroOrMore(Printer.prefix(", "), Printer.newline)
          .print(List(1, 2, 3, 4), "")
          .value == "1, 2, 3, 4\n"
      )
    }

    "repeat a printer several times" in {
      assert(Printer.whitespace.repeat(2).print((), "").value == "  ")
      assert(Printer.int.repeat(2).print(1, "").value == "11")
      assert(Printer.literal("Hi").repeat(2).print("Hi", "").value == "HiHi")

      val e = Printer.literal("Hi").repeat(2).print("Hello", "").left.value
      assert(e.expected == "Hi")
      assert(e.actual == "Hello")
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

      assert(role.print(0, "").value == "admin")
      assert(role.print(1, "").value == "guest")
      assert(role.print(2, "").value == "member")

      val e = role.print(3, "").left.value
      println(e)
      assert(e.expected == "0 or 1 or 2")
      assert(e.actual == "3")
    }
  }
}
