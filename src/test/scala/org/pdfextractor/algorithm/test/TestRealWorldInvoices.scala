package org.pdfextractor.algorithm.test

import java.text.SimpleDateFormat
import java.util.{Date, Locale, Objects}

import org.apache.commons.configuration.{CompositeConfiguration, PropertiesConfiguration}
import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.config.PdfFilesListing
import org.pdfextractor.algorithm.finder.{AbstractFinderTest, FinderResult}
import org.pdfextractor.algorithm.io.{getInputStreamFromFile, _}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.pdfextractor.db.util.TimeHelper
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.{Map, mutable}

class TestRealWorldInvoices extends AbstractFinderTest {

  "Estonian finders" should "find values from real PDFs" in {
    testLanguage(SupportedLocales.ESTONIA)
  }

  //"Italian finders" should "find values from real PDFs" in {
  //  testLanguage(SupportedLocales.ITALY)
  //}

  def getEndToEndResult(locale: Locale, fileName: String): FinderResult = locale.getLanguage match {
    case SupportedLocales.ESTONIAN_LANG_CODE =>
      getEndToEndEstonianResult(fileName)
    case SupportedLocales.ITALIAN_LANG_CODE =>
      getEndToEndItalianResult(fileName)
    case _ =>
      throw new IllegalArgumentException("Unsupported locale: " + locale)
  }

  def getEndToEndEstonianResult(fileName: String): FinderResult = {
    val inputStream = getInputStreamFromFile(fileName)

    finderFactory.extractEstonian(inputStream)
  }

  def getEndToEndItalianResult(fileName: String): FinderResult = {
    val inputStream = getInputStreamFromFile(fileName)

    finderFactory.extractItalian(inputStream)
  }

  implicit class BoolToInt(val b: Boolean) {
    def toInt = if (b) 1 else 0
  }

  val log: Logger = LoggerFactory.getLogger(classOf[TestRealWorldInvoices])

  val saveStatistics = false

  val correctCandidateProperties:
    mutable.Map[PaymentFieldType, mutable.Buffer[Map[String, Any]]] =
    mutable.Map.empty

  def testLanguage(locale: Locale) = {
    val fileNames = PdfFilesListing.getPdfInvoicesListing(locale)

    fileNames.foreach(testFile(locale, _))

    if (saveStatistics) writeStatisticsToFiles(locale, correctCandidateProperties)
  }

  def loadConfiguration(name: String) = {
    val ret = new CompositeConfiguration

    ret.addConfiguration(new PropertiesConfiguration(name + ".properties"))

    ret
  }

  def testFile(locale: Locale, fileName: String) = {
    val result = getEndToEndResult(locale, fileName)

    val config = loadConfiguration(fileName)

    config.getKeys
      .forEachRemaining(key => {
        val paymentFieldType = PaymentFieldType.valueOf(key)

        val expected: Any = findExpected(config, paymentFieldType, fileName)

        paymentFieldType match {
          case IBAN | VATIN => checkArrayOfValues(result, expected.asInstanceOf[Array[String]], fileName, paymentFieldType)
          case _ => checkValue(result, expected, fileName, paymentFieldType)
        }
      })
  }

  def findExpected(config: CompositeConfiguration,
                   paymentFieldType: PaymentFieldType,
                   fileName: String): Any = {
    paymentFieldType match {
      case PaymentFieldType.NAME | INVOICE_ID | REFERENCE_NUMBER =>
        config.getString(paymentFieldType.toString)
      case ISSUE_DATE | DUE_DATE =>
        val df = new SimpleDateFormat("yyyy-MM-dd")
        df.parse(config.getString(paymentFieldType.toString))
      case IBAN | VATIN =>
        config.getStringArray(paymentFieldType.toString)
      case TOTAL_BEFORE_TAXES =>
        config.getDouble(paymentFieldType.toString)
      case TOTAL =>
        if (config.containsKey("TOTAL")) config.getDouble("TOTAL")
        else throw new IllegalStateException(fileName + ": No expected value for total was found")
      case _ =>
        throw new IllegalArgumentException("Unknown payment field type: " + paymentFieldType)
    }
  }

  def addToTrainingData(paymentFieldType: PaymentFieldType, correctCandidate: Candidate) = {
    if (correctCandidateProperties.get(paymentFieldType).isEmpty) {
      correctCandidateProperties += (paymentFieldType -> mutable.Buffer.empty)
    }

    val trainingExamples: mutable.Buffer[Map[String, Any]] =
      correctCandidateProperties.get(paymentFieldType).get


    val valueInMap: Any = paymentFieldType match {
      case PaymentFieldType.ISSUE_DATE => {
        TimeHelper.format(correctCandidate.value.asInstanceOf[Date])
      }
      case _ => correctCandidate.value
    }

    val example: Map[String, Any] = Map(
      "pageNo" -> correctCandidate.pageNo,
      "x" -> correctCandidate.x,
      "y" -> correctCandidate.y,
      "bold" -> correctCandidate.bold.toInt,
      "textHeight" -> correctCandidate.height,
      "value" -> valueInMap
    )

    trainingExamples += example
  }

  def checkValue(result: FinderResult, expected: Any, fileName: String, paymentFieldType: PaymentFieldType) = {
    if (expected != null) {
      assert(result.getCandidates(paymentFieldType).nonEmpty, fileName + ": Expecting values for " + paymentFieldType + ", but did not find any")

      val correctCandidate = result.getCandidates(paymentFieldType).head
      val correctValue = correctCandidate.value

      val equalErr = fileName + ": Found value for " + paymentFieldType + " '" + correctValue + "' differs from expected '" + expected + "'"
      assert(correctValue == expected, equalErr)

      if (saveStatistics) {
        addToTrainingData(paymentFieldType, correctCandidate)
      }
    }
  }

  def checkArrayOfValues(result: FinderResult,
                         values: Seq[String], fileName: String,
                         paymentFieldType: PaymentFieldType) = {
    Objects.requireNonNull(values)
    require(values.nonEmpty)

    val sizeErr = fileName + ": Expecting " + values.size + " values for " + paymentFieldType + ", but found " + result.getCandidates(paymentFieldType).size

    assert(values.size == result.getCandidates(paymentFieldType).size, sizeErr)

    val strings = result.getCandidates(paymentFieldType).map(_.value.toString)

    val equalErr = fileName + ": Not all expected values " + values + " for " + paymentFieldType + " were found: " + strings

    assert(values.toSet == strings.toSet, equalErr)
  }

}
