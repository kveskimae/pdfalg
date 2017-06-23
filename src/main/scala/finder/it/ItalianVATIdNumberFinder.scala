package finder.it

import java.util.regex.Pattern

import candidate.Candidate
import finder.AbstractFinder
import finder.it.ItalianRegexPatterns._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.VATIN
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.RegexUtils

import scala.collection.mutable.ListBuffer
@Service
class ItalianVATIdNumberFinder extends AbstractFinder(PATTERN_ITALIAN_VATIN, PATTERN_ITALIAN_VATIN_VALUE, true, true) {

  protected override def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Pattern): ListBuffer[Candidate] = {
    val ret: ListBuffer[Candidate] = ListBuffer.empty
    val lines: ListBuffer[String] = RegexUtils.findMatches(parseResult.text, searchPattern)
    for (line <- lines) {
      val foundValues: ListBuffer[String] = RegexUtils.findMatches(line, valuePattern)
      for (value <- foundValues) {
        if (isValueAllowed(value)) {
          val candidate: Candidate = buildCandidate(parseResult, null, value)
          addOneElementToListIfNotAlreadyContained(ret, candidate)
        }
      }
    }
    ret
  }

  protected override def buildCandidate(parseResult: ParseResult,
                                        phrase: Phrase,
                                        value: Any,
                                        params: Any*): Candidate = {
    new Candidate(value, 1, 1, false, 1, 1, SupportedLocales.ITALY, VATIN, Map.empty)
  }

  override def isValueAllowed(value: Any): Boolean = {
    value.asInstanceOf[String] != null && value.asInstanceOf[String].length == 11 && value.asInstanceOf[String].matches("\\d*")
  }

  override def parseValue(raw: String): Any = raw

  override def getType: PaymentFieldType = VATIN

}
