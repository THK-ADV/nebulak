package printer

case class PrintingError(expected: String, actual: String) extends Throwable {
  override def getLocalizedMessage = getMessage

  override def getMessage =
    s"""
       |Parsing error:
       |
       |Expected:
       |$expected
       |
       |Actual:
       |$actual""".stripMargin
}
