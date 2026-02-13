package parser

case class ParsingError(expected: String, found: String) extends Throwable {

  override def fillInStackTrace(): Throwable = this

  override def getLocalizedMessage = getMessage

  override def getMessage =
    s"""
       |Parsing error:
       |
       |Expected:
       |$expected
       |
       |Found:
       |$found""".stripMargin
}
