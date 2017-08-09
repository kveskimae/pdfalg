package org.pdfextractor.algorithm.finder.it

import java.util.Locale

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
  extends AbstractFinder(SupportedLocales.ITALY, VATIN, ItVatinR, ItVatinValueR, true, true) {

 override def searchValuesFromPhrase(
                                       phrase: Phrase,
                                       parseResult: ParseResult,
                                       valuePattern2: Regex): mutable.Buffer[Candidate] = {
    searchPattern.get
      .findAllIn(parseResult.text)
      .map(getValuePattern.findAllIn(_))
      .filter(isValueAllowed(_))
      .map(buildCandidate1(_))
      .toBuffer
  }

  override def isValueAllowed(value: Any): Boolean = {
    Option(value).isDefined &&
      value.isInstanceOf[String] &&
      value.asInstanceOf[String].length == 11 &&
      value.asInstanceOf[String].matches("""\d*""")
  }

}
