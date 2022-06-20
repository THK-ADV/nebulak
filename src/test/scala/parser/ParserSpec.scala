package parser

import org.scalatest.wordspec._
import org.scalatest.{EitherValues, OptionValues}
import parser.Parser._
import parser.ParserOps._

class ParserSpec extends AnyWordSpec with EitherValues with OptionValues {

  "A Parser" when {
    "parse a char value" should {
      "return the first char in the input string" in {
        val (res, rest) = char.parse("ab")
        assert(res.value == 'a')
        assert(rest == "b")

        val (res1, rest1) = char.parse("a")
        assert(res1.value == 'a')
        assert(rest1.isEmpty)
      }

      "fail if the input is empty" in {
        val (res, rest) = char.parse("")
        assert(rest.isEmpty)
        res match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "a char")
            assert(e.found == "")
        }
      }
    }

    "parse a sign" should {
      "return 1 if the first char is a plus" in {
        val (res, rest) = sign.parse("+a")
        assert(res.value == 1)
        assert(rest == "a")
      }

      "return -1 if the first char is a minus" in {
        val (res, rest) = sign.parse("-b")
        assert(res.value == -1)
        assert(rest == "b")
      }

      "fail if the first char is something else" in {
        val (res, rest) = sign.parse("ab")
        assert(rest == "ab")
        res match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "+ or -")
            assert(e.found == "ab")
        }
      }
    }

    "parse whitespace" should {

      "parse leading whitespace" in {
        val (res1, rest1) = whitespace.parse(" abc")
        assert(res1.isRight)
        assert(rest1 == "abc")
      }

      "fail if the first char is no whitespace" in {
        val (res2, rest2) = whitespace.parse("abc")
        res2 match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "whitespace")
            assert(e.found == "abc")
            assert(rest2 == "abc")
        }
      }
    }

    "parse zero or more whitespace" should {
      "parse leading whitespace" in {
        val (res1, rest1) = zeroOrMoreSpaces.parse(" abc")
        assert(res1.isRight)
        assert(rest1 == "abc")
      }

      "parse leading whitespaces" in {
        val (res2, rest2) = zeroOrMoreSpaces.parse("   abc")
        assert(res2.isRight)
        assert(rest2 == "abc")
      }

      "parse leading whitespace even if there is none" in {
        val (res3, rest3) = zeroOrMoreSpaces.parse("abc")
        assert(res3.isRight)
        assert(rest3 == "abc")
      }
    }

    "parse a newline" should {

      "parse \\n as a newline" in {
        val (res1, rest1) = newline.parse("\nabc")
        assert(res1.isRight)
        assert(rest1 == "abc")

        val (res2, rest2) = newline.parse("\n")
        assert(res2.isRight)
        assert(rest2.isEmpty)
      }

      "parse \\r\\n as a newline" in {
        val (res3, rest3) = newline.parse("\r\nabc")
        assert(res3.isRight)
        assert(rest3 == "abc")

        val (res4, rest4) = newline.parse("\r\n")
        assert(res4.isRight)
        assert(rest4.isEmpty)
      }

      "fail if there is no leading newline" in {
        val (res5, rest5) = newline.parse("abc")
        res5 match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "newline")
            assert(e.found == "abc")
            assert(rest5 == "abc")
        }
      }
    }

    "parse a int value" should {
      "return a leading int value" in {
        val (res, rest) = int.parse("123 Hello")
        assert(res.value == 123)
        assert(rest == " Hello")
      }

      "return a leading negative int value" in {
        val (res, rest) = int.parse("-123 Hello")
        assert(res.value == -123)
        assert(rest == " Hello")
      }

      "return a leading positive int value" in {
        val (res, rest) = int.parse("+123 Hello")
        assert(res.value == 123)
        assert(rest == " Hello")
      }

      "fail if there is no leading int value" in {
        val (res1, rest1) = int.parse("-Hello")
        assert(rest1 == "-Hello")
        res1 match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "an integer")
            assert(e.found == "-Hello")
        }

        val (res2, rest2) = int.parse("Hello Blob")
        assert(rest2 == "Hello Blob")
        res2 match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "an integer")
            assert(e.found == "Hello Blob")
        }
      }
    }

    "parse a double value" should {

      "return a leading double value with no decimal value" in {
        val (res, rest) = double.parse("42 Hello")
        assert(res.value == 42)
        assert(rest == " Hello")
      }

      "return a leading double value with decimal value" in {
        val (res, rest) = double.parse("4.2 Hello")
        assert(res.value == 4.2)
        assert(rest == " Hello")
      }

      "return a leading double value with trailing decimal value" in {
        val (res, rest) = double.parse("42. Hello")
        assert(res.value == 42)
        assert(rest == " Hello")
      }

      "return a leading double value with leading decimal value" in {
        val (res, rest) = double.parse(".42 Hello")
        assert(res.value == 0.42)
        assert(rest == " Hello")
      }

      "return a leading negative double value with no decimal value" in {
        val (res, rest) = double.parse("-42 Hello")
        assert(res.value == -42)
        assert(rest == " Hello")
      }

      "return a leading positive double value with no decimal value" in {
        val (res, rest) = double.parse("+42 Hello")
        assert(res.value == 42)
        assert(rest == " Hello")
      }

      "return a leading double value after decimal split" in {
        val (res, rest) = double.parse("1.2.3 Hello")
        assert(res.value == 1.2)
        assert(rest == ".3 Hello")
      }
    }

    "parse a prefix" should {
      "expect a given prefix in input string" in {
        val (res, rest) = prefix("He").parse("Hello")
        assert(res.isRight)
        assert(rest == "llo")

        val (res1, rest1) = prefix("He").parse("Hello World")
        assert(res1.isRight)
        assert(rest1 == "llo World")
      }

      "fail if the input string is empty" in {
        val (res, rest) = prefix("He").parse("")
        assert(rest == "")
        res match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "He")
            assert(e.found == "")
        }
      }

      "fail if the input string doesn't start with the prefix" in {
        val (res, rest) = prefix("He").parse("World Hello")
        assert(rest == "World Hello")
        res match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "He")
            assert(e.found == "World Hello")
        }
      }
    }

    "always succeed with a parser" in {
      val (res, rest) = always(1).parse("Hello")
      assert(res.value == 1)
      assert(rest == "Hello")

      val (res1, rest1) = always(1).parse("")
      assert(res1.value == 1)
      assert(rest1.isEmpty)
    }

    "never succeed with a parser" in {
      val (res1, rest1) = never().parse("Hello")
      assert(rest1 == "Hello")
      res1 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "fail")
          assert(e.found == "Hello")
      }

      val (res2, rest2) = never().parse("")
      assert(rest2 == "")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "fail")
          assert(e.found == "")
      }
    }

    "map a parser" should {

      "preserve the output if mapping over identity" in {
        val int0 = int.map(identity)
        assert(int0.parse("123") == int.parse("123"))
      }

      "map the output of a parser" in {
        val even = int.map(_ % 2 == 0)

        val (res1, rest1) = even.parse("24 Hello")
        assert(res1.value)
        assert(rest1 == " Hello")

        val (res2, rest2) = even.parse("23 Hello")
        assert(!res2.value)
        assert(rest2 == " Hello")

        val (res3, rest3) = even.parse("Hello")
        assert(rest3 == "Hello")
        res3 match {
          case Right(_) => fail()
          case Left(e) =>
            assert(e.expected == "an integer")
            assert(e.found == "Hello")
        }
      }
    }

    "flatMap a parser and recover the original string properly" in {
      val even: Parser[Int] = int.flatMap(a =>
        if (a % 2 == 0) always(a)
        else never("even")
      )
      val (res, rest) = even.parse("24 Hello")
      assert(res.value == 24)
      assert(rest == " Hello")

      val (res1, rest1) = even.parse("23 Hello")
      assert(rest1 == "23 Hello")
      res1 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "even")
          assert(e.found == "23 Hello")
      }

      val (res2, rest2) = even.parse("Hello")
      assert(rest2 == "Hello")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "an integer")
          assert(e.found == "Hello")
      }
    }

    "zip two parsers together" in {
      val money = int.skip(prefix(" €"))
      val (res, rest) = money.parse("12 €")
      assert(res.value == 12)
      assert(rest.isEmpty)

      val (res1, rest1) = money.parse("12€")
      assert(rest1 == "12€")
      res1 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == " €")
          assert(e.found == "12€")
      }

      val (res2, rest2) = money.parse("12 $")
      assert(rest2 == "12 $")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == " €")
          assert(e.found == "12 $")
      }

      val (res3, rest3) = money.parse("12")
      assert(rest3 == "12")
      res3 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == " €")
          assert(e.found == "12")
      }
    }

    "take one of many parsers" in {
      val currency: Parser[String] = oneOf(
        prefix("€").map(_ => "EUR"),
        prefix("$").map(_ => "USD"),
        prefix("£").map(_ => "GBP")
      )

      val (res1, rest1) = currency.parse("€$abc")
      assert(res1.value == "EUR")
      assert(rest1 == "$abc")

      val (res2, rest2) = currency.parse("$£abc")
      assert(res2.value == "USD")
      assert(rest2 == "£abc")

      val (res3, rest3) = currency.parse("£abc")
      assert(res3.value == "GBP")
      assert(rest3 == "abc")

      val (res4, rest4) = currency.parse("abc")
      assert(rest4 == "abc")
      res4 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "€ or $ or £")
          assert(e.found == "abc")
      }
    }

    "parse zero or more parsers with separator" in {
      val ints = int.many(prefix(","))

      val (res1, rest1) = ints.parse("1,2,3,4,5")
      assert(res1.value == List(1, 2, 3, 4, 5))
      assert(rest1.isEmpty)

      val (res2, rest2) = ints.parse("1,2,,3,4,5")
      assert(res2.value == List(1, 2))
      assert(rest2 == ",3,4,5")

      val (res3, rest3) = ints.parse(",1,2,,3,4,5")
      assert(res3.value == Nil)
      assert(rest3 == ",1,2,,3,4,5")
    }

    "parse zero or more parsers with terminator" in {
      val ints = int.many(prefix(","), end)

      val (res1, rest1) = ints.parse("1,2,3,4,5")
      assert(res1.value == List(1, 2, 3, 4, 5))
      assert(rest1.isEmpty)

      val (res2, rest2) = ints.parse("1,2,3,4,5 ")
      assert(res2.isLeft)
      assert(rest2 == "1,2,3,4,5 ")
    }

    "parse at least one element" in {
      val ints = int.many(separator = prefix(","), minimum = 1)

      val (res, rest) = ints.parse("1")
      assert(rest.isEmpty)
      assert(res.value == List(1))

      val (res1, rest1) = ints.parse("1,2")
      assert(rest1.isEmpty)
      assert(res1.value == List(1, 2))

      val (res2, rest2) = ints.parse("abc")
      val e = res2.left.value
      assert(rest2 == "abc")
      assert(e.expected == "minimum of 1 elements")
      assert(e.found == "abc")
    }

    "parse at least two elements" in {
      val ints = int.many(separator = prefix(","), minimum = 2)

      val (res1, rest1) = ints.parse("1,2")
      assert(rest1.isEmpty)
      assert(res1.value == List(1, 2))

      val (res2, rest2) = ints.parse("abc")
      val e = res2.left.value
      assert(rest2 == "abc")
      assert(e.expected == "minimum of 2 elements")
      assert(e.found == "abc")

      val (res, rest) = ints.parse("1")
      val e2 = res.left.value
      assert(rest == "1")
      assert(e2.expected == "minimum of 2 elements")
      assert(e2.found == "")
    }

    "skip the result of the first parser" in {
      val (res, rest) = prefix("$").take(int).parse("$1abc")
      assert(res.value == 1)
      assert(rest == "abc")

      val (res1, rest1) = skipFirst(prefixTo("..")).take(int).parse("ab..1abc")
      assert(res1.value == 1)
      assert(rest1 == "abc")
    }

    "parse string to a token" in {
      val p = prefixTo(" : ")

      val (res1, rest1) = p.parse("abc : def")
      assert(res1.value == "abc")
      assert(rest1 == "def")

      val (res2, rest2) = p.parse("abc: def")
      assert(rest2 == "abc: def")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == " : ")
          assert(e.found == "abc: def")
      }
    }

    "parse a string until a token is found" in {
      val p = prefixUntil(",")

      val (res1, rest1) = p.parse("abc,def")
      assert(res1.value == "abc")
      assert(rest1 == ",def")

      val (res2, rest2) = p.parse("abcdef")
      assert(rest2 == "abcdef")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == ",")
          assert(e.found == "abcdef")
      }
    }

    "parse one or another" in {
      val aOrB = prefix("a") or prefix("b")

      val (res1, rest1) = aOrB.parse("abc")
      assert(res1.isRight)
      assert(rest1 == "bc")

      val (res2, rest2) = aOrB.parse("bac")
      assert(res2.isRight)
      assert(rest2 == "ac")

      val (res3, rest3) = aOrB.parse("cab")
      assert(rest3 == "cab")
      res3 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "a or b")
          assert(e.found == "cab")
      }
    }

    "parse the end of an input string" in {
      val (res1, rest1) = end.parse("")
      assert(res1.isRight)
      assert(rest1.isEmpty)

      val (res2, rest2) = end.parse("abc")
      assert(rest2 == "abc")
      res2 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "end of input")
          assert(e.found == "abc")
      }

      val (res3, rest3) = end.parse("a")
      assert(rest3 == "a")
      res3 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "end of input")
          assert(e.found == "a")
      }

      val (res4, rest4) = end.parse(" ")
      assert(rest4 == " ")
      res4 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "end of input")
          assert(e.found == " ")
      }
    }

    "optionally parse if the parser succeeds" in {
      val p = optional(prefix("-"))

      val (res1, rest1) = p.parse("-1")
      assert(res1.isRight)
      assert(rest1 == "1")

      val (res2, rest2) = p.parse("1")
      assert(res2.isRight)
      assert(rest2 == "1")
    }

    "expect a parser to fail which is the success case" in {
      val p = not(prefix("."))

      val (res1, rest1) = p.parse("abc def")
      assert(res1.isRight)
      assert(rest1 == "abc def")

      val (res2, rest2) = p.parse("")
      assert(res2.isRight)
      assert(rest2.isEmpty)

      val (res3, rest3) = p.parse(".abc")
      assert(rest3 == ".abc")
      res3 match {
        case Right(_) => fail()
        case Left(e) =>
          assert(e.expected == "not to succeed")
          assert(e.found == ".abc")
      }
    }

    "parse the rest of the string" in {
      val (res1, rest1) = rest.parse("abc")
      assert(res1.value == "abc")
      assert(rest1.isEmpty)
    }

    "parse maybe an int" in {
      val (res1, rest1) = int.option.parse("123 hello")
      assert(rest1 == " hello")
      assert(res1.value.value == 123)

      val (res2, rest2) = int.option.parse("hello")
      assert(rest2 == "hello")
      assert(res2.value.isEmpty)
    }
  }
}
