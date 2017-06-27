package finder

import org.junit.runner.RunWith
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.ActiveProfiles
import org.springframework.test.context.ContextConfiguration
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner
import java.io.InputStream
import java.util.Locale

import config.ExtractorConfig
import org.pdfextractor.db.config.{JpaConfig, StandaloneDataConfig}
import finder.FinderFactory
import finder.FinderResult
import io.IOHelper
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.beans.factory.annotation.Autowired
import java.io.InputStream
import java.util.Locale

import org.scalatest.{FlatSpec, Matchers}

object AbstractInvoiceFileReader {

  val Starman = "et/Starman.pdf"

}

@RunWith(classOf[SpringJUnit4ClassRunner])
@ContextConfiguration(classes=Array(classOf[JpaConfig], classOf[StandaloneDataConfig], classOf[ExtractorConfig]))
@ActiveProfiles(Array("unittest"))
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
