package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate._
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.TOTAL
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import org.pdfextractor.algorithm.finder._

@Service
class EstonianTotalFinder extends AbstractFinder {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("^(?ism)(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, TOTAL) + "(.*)$").r)
    valuePattern = Some(DigitsAndCommasR)
  }

  override protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Regex): ListBuffer[Candidate] = {
    val ret = scala.collection.mutable.ListBuffer.empty[Candidate]
    val doubleValues = searchForEstonianDoubleValuesAfterText(phrase.text)
    if (doubleValues.size == 1) {
      val totalAsNumberMatcher = DigitsAndCommasR.findAllIn(phrase.text)
      while ( {
        totalAsNumberMatcher.hasNext
      }) {
        var totalAsString = totalAsNumberMatcher.next()
        val dotCount = countDotsAndCommas(totalAsString)
        if (dotCount < 2) {
          totalAsString = totalAsString.replaceAll(",", ".")
          val doubleNumber = dotCount > 0
          val totalAsDouble = parseValue(totalAsString).asInstanceOf[Double]
          val `type` = phraseTypesStore.findType(SupportedLocales.ESTONIA, TOTAL, phrase.text)
          val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
          ret += candidate
        }
      }
    }
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val doubleNumber = params(0).asInstanceOf[Boolean]
    val `type` = params(1).asInstanceOf[PhraseType]
    val euroSignFound = isEuroPresent(phrase.text)
    val normalTotalLine = isNormalTotalLine(phrase.text)
    val properties: Map[CandidateMetadata, Any] = Map(IsDouble -> doubleNumber, MetaPhraseType -> `type`, HasEuroSign -> euroSignFound, IsNormalLine -> normalTotalLine)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, TOTAL, properties)
    ret
  }

  override def isValueAllowed(value: Any) = true

  override def parseValue(raw: String): Any = raw.replace(',', '.').toDouble

  override def getType = TOTAL

}
