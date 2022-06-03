package parser

case class ParsingError(expected: String, remainingInput: String)
    extends Throwable {

  override def getLocalizedMessage = getMessage

  override def getMessage =
    s"""
       |Parsing error:
       |
       |Expected:
       |$expected
       |
       |Found:
       |$remainingInput""".stripMargin
}
