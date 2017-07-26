package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate._
import org.pdfextractor.algorithm.finder.AbstractFinder
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

import scala.collection.mutable

@Service
class EstonianTotalFinder extends AbstractFinder {

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ism)(.*)" + phraseTypesStore
        .buildAllPhrases(SupportedLocales.ESTONIA, TOTAL) + "(.*)$").r)
    valuePattern = Some(DigitsAndCommasR)
  }

  override protected def searchValuesFromPhrase(
      phrase: Phrase,
      parseResult: ParseResult,
      valuePattern2: Regex): mutable.Buffer[Candidate] = {
    if (searchForEstonianDoubleValuesAfterText(phrase.text).size == 1) {
      def totalString2Candidate: String => Candidate =
        (totalAsString: String) => {
          val doubleNumber = countDotsAndCommas(totalAsString) > 0
          val totalAsDouble = parseValue(totalAsString).asInstanceOf[Double]
          val phraseType = phraseTypesStore.findType(SupportedLocales.ESTONIA,
                                                     TOTAL,
                                                     phrase.text)

          buildCandidate(parseResult,
                         phrase,
                         totalAsDouble,
                         doubleNumber,
                         phraseType)
        }

      DigitsAndCommasR
        .findAllIn(phrase.text)
        .filter(countDotsAndCommas(_) < 2)
        .map(_.replaceAll(",", "."))
        .map(totalString2Candidate)
        .toBuffer
    } else {
      ListBuffer.empty
    }
  }

  def buildProperties(phrase: Phrase,
                      params: Seq[Any]): Map[CandidateMetadata, Any] = {
    val doubleNumber = params(0).asInstanceOf[Boolean]
    val phraseType = params(1).asInstanceOf[PhraseType]
    val euroSignFound = isEuroPresent(phrase.text)
    val normalTotalLine = isNormalTotalLine(phrase.text)

    Map(
      IsDouble -> doubleNumber,
      MetaPhraseType -> phraseType,
      HasEuroSign -> euroSignFound,
      IsNormalLine -> normalTotalLine
    )
  }

  override protected def buildCandidate(parseResult: ParseResult,
                                        phrase: Phrase,
                                        value: Any,
                                        params: Any*): Candidate = {
    new Candidate(value,
                  phrase.x,
                  phrase.y,
                  phrase.bold,
                  phrase.height,
                  phrase.pageNumber,
                  SupportedLocales.ESTONIA,
                  TOTAL,
                  buildProperties(phrase, params))
  }

  override def isValueAllowed(value: Any) = true

  override def parseValue(raw: String): Any = raw.replace(',', '.').toDouble

  override def getType = TOTAL

}
