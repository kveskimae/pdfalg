package finder.it

import org.pdfextractor.algorithm.candidate.Candidate
import finder.AbstractFinder
import finder.it.ItalianRegexPatterns._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.VATIN
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
@Service
class ItalianVATIdNumberFinder extends AbstractFinder(PATTERN_ITALIAN_VATIN_AS_REGEX, PATTERN_ITALIAN_VATIN_VALUE_AS_REGEX, true, true) {

  protected override def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Regex): ListBuffer[Candidate] = {
    val ret: ListBuffer[Candidate] = ListBuffer.empty
    val lines: Regex.MatchIterator = getSearchPattern.findAllIn(parseResult.text)
    for (line <- lines) {
      val foundValues: Regex.MatchIterator = getValuePattern.findAllIn(line)
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
    value.asInstanceOf[String] != null && value.asInstanceOf[String].length == 11 && value.asInstanceOf[String].matches("""\d*""")
  }

  override def parseValue(raw: String): Any = raw

  override def getType: PaymentFieldType = VATIN

}
