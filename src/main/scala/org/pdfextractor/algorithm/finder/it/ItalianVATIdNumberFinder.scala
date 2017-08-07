package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.VATIN
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.stereotype.Service

import scala.collection.mutable
import scala.util.matching.Regex

@Service
class ItalianVATIdNumberFinder
  extends AbstractFinder(ItVatinR, ItVatinValueR, true, true) {

  override def searchValuesFromPhrase(
                                       phrase: Phrase,
                                       parseResult: ParseResult,
                                       valuePattern2: Regex): mutable.Buffer[Candidate] = {
    searchPattern.get
      .findAllIn(parseResult.text)
      .map(getValuePattern.findAllIn(_))
      .filter(isValueAllowed(_))
      .map(buildCandidate(parseResult, None, _))
      .toBuffer
  }

  def buildCandidate(parseResult: ParseResult,
                     phrase: Option[Phrase],
                     value: Any,
                     params: Any*): Candidate = {
    buildCandidate(phrase.orNull, value, params)
  }

  override def buildCandidate(phrase: Phrase,
                              value: Any,
                              params: Any*): Candidate = {
    new Candidate(value,
      1,
      1,
      false,
      1,
      1,
      SupportedLocales.ITALY,
      VATIN,
      Map.empty)
  }

  override def isValueAllowed(value: Any): Boolean = {
    Option(value).isDefined &&
      value.isInstanceOf[String] &&
      value.asInstanceOf[String].length == 11 &&
      value.asInstanceOf[String].matches("""\d*""")
  }

  override def parseValue(raw: String): Any = raw

  override def getType: PaymentFieldType = VATIN

}
