package org.pdfextractor.algorithm.finder.it

import java.text.{DecimalFormat, DecimalFormatSymbols, ParseException}
import java.util.Locale

import org.pdfextractor.algorithm.candidate._
import org.pdfextractor.algorithm.finder.{AbstractFinder, _}
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.stereotype.Service

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

@Service
abstract class AbstractItalianTotalFinder(finderFieldType: PaymentFieldType)
  extends AbstractFinder(SupportedLocales.ITALY, finderFieldType, None, None, true, true) {

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
  val ordinaryTotalLineR = ("""^(?ims).{0,30}:([\s]{0,})""" + Eur + """?([\s]{0,})""" + DigitsAndCommas + """([\s]{0,})""" + Eur + """?([\s]{0,})$""").r

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ims)(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, finderFieldType) + "(.*)$").r)
    valuePattern = Some(DigitsAndCommasR)
  }

  override def searchValuesFromPhrase(
                                       phrase: Phrase,
                                       parseResult: ParseResult,
                                       valuePattern2: Regex): mutable.Buffer[Candidate] = {
    val doubleValues = searchForDoubleValues(phrase.text)
    if (doubleValues.size == 1) {
      DigitsAndCommasR
        .findAllIn(phrase.text)
        .map(findCandidateValue(_, phrase, parseResult))
        .filter(_.isDefined)
        .map(_.get)
        .toBuffer
    } else {
      val all = DigitsAndCommasR
        .findAllIn(phrase.text)
        .map(findCandidateValue(_, phrase, parseResult))
        .filter(_.isDefined)
        .map(_.get)

      if (all.nonEmpty) {
        val getBigger = (candidate1: Candidate, candidate2: Candidate) => if (candidate1.getValue.asInstanceOf[Double] > candidate2.getValue.asInstanceOf[Double]) candidate1 else candidate2

        mutable.Buffer(all.reduce(getBigger))
      } else {
        mutable.Buffer.empty
      }
    }
  }

  def findCandidateValue(
                          totalAsString: String,
                          phrase: Phrase,
                          parseResult: ParseResult): Option[Candidate] = {
    val dotCount = countDotsAndCommas(totalAsString)
    val phraseType =
      phraseTypesStore.findType(SupportedLocales.ITALY, finderFieldType, phrase.text)
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

  override def buildProperties(phrase: Phrase, parseResult: ParseResult, params: Seq[Any]): Map[CandidateFeatureType, Any] = {
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
    ordinaryTotalLineR.findFirstIn(text).nonEmpty
  }

  override def parseValue(raw: String) = throw new UnsupportedOperationException

}
