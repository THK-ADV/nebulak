package printer

case class PrintingError(expected: String, found: String) extends Throwable {

  override def fillInStackTrace(): Throwable = this

  override def getLocalizedMessage = getMessage

  override def getMessage =
    s"""
       |Printing error:
       |
       |Expected:
       |$expected
       |
       |Found:
       |$found""".stripMargin
}
