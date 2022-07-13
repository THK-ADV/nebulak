package printer

case class PrintingError(expected: String, found: String) extends Throwable {
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
