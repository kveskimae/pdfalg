package org.pdfextractor.algorithm.finder.it

import java.text.{DecimalFormat, DecimalFormatSymbols, ParseException}
import java.util.Locale

import org.pdfextractor.algorithm.candidate._
import org.pdfextractor.algorithm.finder.{AbstractFinder, _}
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

@Service
abstract class AbstractItalianTotalFinder
  extends AbstractFinder(None, None, true, true) {

  val otherSymbolsForCommaAsThousandsSeparator: DecimalFormatSymbols =
    new DecimalFormatSymbols(Locale.ITALY)

  // TODO What about thread safety?
  val decimalFormatWithCommaAsThousandsSeparator: DecimalFormat =
    new DecimalFormat("###,###.##", otherSymbolsForCommaAsThousandsSeparator)
  otherSymbolsForCommaAsThousandsSeparator.setDecimalSeparator('.')
  otherSymbolsForCommaAsThousandsSeparator.setGroupingSeparator(',')
  val otherSymbolsForDotAsThousandsSeparator: DecimalFormatSymbols =
    new DecimalFormatSymbols(Locale.ITALY)
  // Even if dot is used as radix character (decimal separator), pattern is always defined with comma as separator
  val decimalFormatWithDotAsThousandsSeparator: DecimalFormat =
    new DecimalFormat("###,###.##", otherSymbolsForDotAsThousandsSeparator)
  otherSymbolsForDotAsThousandsSeparator.setDecimalSeparator(',')
  otherSymbolsForDotAsThousandsSeparator.setGroupingSeparator('.')
  private[it] val PATTERN_ITALIAN_ORDINARY_TOTAL_LINE_AS_REGEX =
    ("""^(?ims).{0,30}:([\s]{0,})""" + Eur + """?([\s]{0,})""" + DigitsAndCommas + """([\s]{0,})""" + Eur + """?([\s]{0,})$""").r

  override def getLocale: Locale = SupportedLocales.ITALY

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ims)(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY,
        getType) + "(.*)$").r)
    valuePattern = Some(DigitsAndCommasR)
  }

  override def searchValuesFromPhrase(
                                       phrase: Phrase,
                                       parseResult: ParseResult,
                                       valuePattern2: Regex): mutable.Buffer[Candidate] = {
    val ret: ListBuffer[Candidate] = ListBuffer.empty
    val doubleValues = searchForDoubleValues(phrase.text)
    if (doubleValues.size == 1) {
      val totalAsNumberMatcher = DigitsAndCommasR.findAllIn(phrase.text)
      while ( {
        totalAsNumberMatcher.hasNext
      }) {
        val totalAsString = totalAsNumberMatcher.next()
        val candidate: Option[Candidate] =
          findCandidateValue(totalAsString, phrase, parseResult)
        if (candidate.isDefined) ret += candidate.get
      }
    } else {
      val totalAsNumberMatcher = DigitsAndCommasR.findAllIn(phrase.text)
      var biggest: Option[Candidate] = None
      while ( {
        totalAsNumberMatcher.hasNext
      }) {
        val totalAsString = totalAsNumberMatcher.next()
        val candidate: Option[Candidate] =
          findCandidateValue(totalAsString, phrase, parseResult)
        if (biggest.isEmpty) biggest = candidate
        else if (Option(candidate).isDefined && candidate.get.value
          .asInstanceOf[Double] > biggest.get.value
          .asInstanceOf[Double]) biggest = candidate
      }
      if (biggest.isDefined) ret += biggest.get
    }
    ret
  }

  def findCandidateValue(
                          totalAsString: String,
                          phrase: Phrase,
                          parseResult: ParseResult): Option[Candidate] = {
    val dotCount = countDotsAndCommas(totalAsString)
    val phraseType =
      phraseTypesStore.findType(SupportedLocales.ITALY, getType, phrase.text)
    if (dotCount < 2) {
      val replaced = totalAsString.replaceAll(",", ".")
      val totalAsDouble: Double = replaced.toDouble
      val doubleNumber = dotCount > 0
      val candidate =
        buildCandidate(phrase, parseResult, totalAsDouble, Seq(doubleNumber, phraseType))
      Some(candidate)
    } else
      try {
        var totalAsDouble = .0
        if (isDotThousandsSeparator(totalAsString))
          totalAsDouble = decimalFormatWithDotAsThousandsSeparator
            .parse(totalAsString)
            .doubleValue
        else
          totalAsDouble = decimalFormatWithCommaAsThousandsSeparator
            .parse(totalAsString)
            .doubleValue
        val doubleNumber = isDouble(totalAsDouble)
        val candidate = buildCandidate(phrase,
          parseResult,
          totalAsDouble,
          Seq(doubleNumber, phraseType))
        Some(candidate)
      } catch {
        case ignored: ParseException =>
          None
      }
  }

  def isDotThousandsSeparator(totalAsString: String): Boolean = {
    totalAsString.contains(",") &&
      totalAsString.contains(".") &&
      (totalAsString.indexOf('.') < totalAsString.indexOf(','))
  }

  def isDouble(number: Double): Boolean = (number % 1) != 0

  override def buildProperties(phrase: Phrase, parseResult: ParseResult, params: Seq[Any]): Map[CandidateMetadata, Any] = {
    val doubleNumber = params(0).asInstanceOf[Boolean]
    val phraseType = params(1).asInstanceOf[PhraseType]
    val euroSignFound = isEuroPresent(phrase.text)
    val normalTotalLine = isNormalTotalLine(phrase.text)

    Map(IsDouble -> doubleNumber,
      MetaPhraseType -> phraseType,
      HasEuroSign -> euroSignFound,
      IsNormalLine -> normalTotalLine)
  }

  def isEuroPresent(text: String): Boolean = {
    EurR.findFirstIn(text).nonEmpty
  }

  def isNormalTotalLine(text: String): Boolean = {
    PATTERN_ITALIAN_ORDINARY_TOTAL_LINE_AS_REGEX.findFirstIn(text).nonEmpty
  }

  override def isValueAllowed(value: Any) = true

  def parseValue(raw: String) = throw new UnsupportedOperationException

}
