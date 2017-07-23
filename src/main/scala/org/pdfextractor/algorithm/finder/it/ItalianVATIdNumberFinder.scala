package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.VATIN
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
@Service
class ItalianVATIdNumberFinder extends AbstractFinder(ItVatinR, ItVatinValueR, true, true) {

  protected override def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Regex): ListBuffer[Candidate] = {
    val ret: ListBuffer[Candidate] = ListBuffer.empty
    val lines: Regex.MatchIterator = getSearchPattern.findAllIn(parseResult.text)
    for (line <- lines) {
      val foundValues: Regex.MatchIterator = getValuePattern.findAllIn(line)
      for (value <- foundValues) {
        if (isValueAllowed(value)) {
          val candidate: Candidate = buildCandidate(parseResult, None, value)
          addOneElementToListIfNotAlreadyContained(ret, candidate)
        }
      }
    }
    ret
  }
  protected def buildCandidate(parseResult: ParseResult, phrase: Option[Phrase], value: Any, params: Any*): Candidate = buildCandidate(parseResult, phrase.orNull, value, params)

  protected override def buildCandidate(parseResult: ParseResult,
                                        phrase: Phrase,
                                        value: Any,
                                        params: Any*): Candidate = {
    new Candidate(value, 1, 1, false, 1, 1, SupportedLocales.ITALY, VATIN, Map.empty)
  }

  override def isValueAllowed(value: Any): Boolean = {
    Option(value).isDefined && value.isInstanceOf[String] && value.asInstanceOf[String].length == 11 && value.asInstanceOf[String].matches("""\d*""")
  }

  override def parseValue(raw: String): Any = raw

  override def getType: PaymentFieldType = VATIN

}
