package finder

object AbstractInvoiceFileReader {

  val Starman = "et/Starman.pdf"

}
/*
trait AbstractInvoiceFileReader  {

  @Autowired var finderFactory: FinderFactory = _

  def getEndToEndResult(locale: Locale, fileName: String): FinderResult = locale.getLanguage match {
    case SupportedLocales.ESTONIAN_LANG_CODE =>
      getEndToEndEstonianResult(fileName)
    case SupportedLocales.ITALIAN_LANG_CODE =>
      getEndToEndItalianResult(fileName)
    case _ =>
      throw new IllegalArgumentException("Unsupported locale: " + locale)
  }

  def getEndToEndEstonianResult(fileName: String): FinderResult = {
    val inputStream = IOHelper.getInputStreamFromFile(fileName)
    val result = finderFactory.extractEstonian(inputStream)
    result
  }

  def getEndToEndItalianResult(fileName: String): FinderResult = {
    val inputStream = IOHelper.getInputStreamFromFile(fileName)
    val result = finderFactory.extractItalian(inputStream)
    result
  }

}
*/