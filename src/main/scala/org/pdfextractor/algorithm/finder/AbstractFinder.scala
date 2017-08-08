package org.pdfextractor.algorithm.finder

import java.util.{Locale, Objects}

import org.pdfextractor.algorithm.candidate.{Candidate, CandidateMetadata}
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesStore
import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired

import scala.collection.mutable
import scala.util.matching.Regex

abstract class AbstractFinder(var searchPattern: Option[Regex],
                              var valuePattern: Option[Regex],
                              val combinePhrases: Boolean = true,
                              val combineFuzzy: Boolean = false) {

  val log: Logger = LoggerFactory.getLogger(classOf[AbstractFinder])
  @Autowired var phraseTypesStore: PhraseTypesStore = _

  def this() = this(None, None, true, false)

  def this(searchPattern2: Regex, valuePattern2: Regex) =
    this(Some(searchPattern2), Some(valuePattern2))

  def this(searchPattern2: Regex,
           valuePattern2: Regex,
           combinePhrases2: Boolean) =
    this(Some(searchPattern2), Some(valuePattern2), combinePhrases2)

  def this(searchPattern2: Regex,
           valuePattern2: Regex,
           combinePhrases2: Boolean,
           combineFuzzy2: Boolean) =
    this(Some(searchPattern2),
      Some(valuePattern2),
      combinePhrases2,
      combineFuzzy2)

  def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    searchWithPattern(parseResult, searchPattern.get, getValuePattern)
  }

  def combineAndSearch(first: Phrase,
                       second: Phrase,
                       parseResult: ParseResult,
                       value: Regex): Seq[Candidate] = {
    val combined = combinePhrases1(first, second)
    searchValuesFromPhrase(combined, parseResult, value)
  }

  def searchWithRight(phrase: Phrase,
                      parseResult: ParseResult,
                      value: Regex): Seq[Candidate] = {
    searchRight(parseResult.findClosestPhraseOnRight(phrase), parseResult) match {
      case Some(right) => combineAndSearch(phrase, right, parseResult, value)
      case None => Nil
    }
  }

  def searchWithBelow(phrase: Phrase,
                      parseResult: ParseResult,
                      value: Regex): Seq[Candidate] = {
    parseResult.findPhraseBelow(phrase) match {
      case Some(phraseBelow) => {
        val combined = combinePhrases1(phrase, phraseBelow)
        searchValuesFromPhrase(combined, parseResult, value) match {
          case combinedResults if combinedResults.isEmpty && TOTAL == getType => {
            parseResult.findPhraseBelow(phraseBelow) match {
              case Some(beneathBelow) => {
                combineAndSearch(combined, beneathBelow, parseResult, value)
              }
              case None => Nil
            }
          }
          case xs => xs
        }
      }
      case None => Nil
    }
  }

  def searchFuzzy(phrase: Phrase,
                  parseResult: ParseResult,
                  value: Regex): Seq[Candidate] = {
    parseResult
      .searchRightOrBelow(phrase)
      .map(closest => combineAndSearch(phrase, closest, parseResult, value))
      .flatten
  }

  def searchWithCombining(phrase: Phrase, parseResult: ParseResult, value: Regex): Seq[Candidate] = {
    searchValuesFromPhrase(phrase, parseResult, value) match {
      case ret if ret.nonEmpty => ret
      case _ => {
        searchWithRight(phrase, parseResult, value) match {
          case ret if ret.nonEmpty => ret
          case _ => {
            // no success in combining with right, try below
            searchWithBelow(phrase, parseResult, value) match {
              case ret if ret.nonEmpty => ret
              case _ if combineFuzzy =>
                // no success with below either, loosen search up
                searchFuzzy(phrase, parseResult, value)
              case _ => Nil
            }
          }
        }
      }
    }
  }

  def searchWithPattern(parseResult: ParseResult,
                        search: Regex,
                        value: Regex): Seq[Candidate] = {
    Objects.requireNonNull(parseResult)

    parseResult.phrases
      .filter(phrase => search.findFirstIn(phrase.text).nonEmpty)
      .map(phrase => searchWithCombining(phrase, parseResult, value))
      .flatten
      .sorted
  }

  def searchValuesFromPhrase(phrase: Phrase,
                             parseResult: ParseResult,
                             valuePattern2: Regex): mutable.Buffer[Candidate] = {
    valuePattern2
      .findAllIn(phrase.text)
      .map(parseValue(_))
      .filter(isValueAllowed(_))
      .map(buildCandidate(phrase, parseResult, _, Seq.empty))
      .toBuffer
  }

  def isValueAllowed(value: Any): Boolean

  def parseValue(raw: String): Any

  def getType: PaymentFieldType

  def getLocale: Locale

  def getValuePattern: Regex = valuePattern.get

  // Building candidate

  def buildCandidate1(value: Any): Candidate =
    new Candidate(value,
      1,
      1,
      false,
      1,
      1,
      getLocale,
      getType,
      Map.empty)

  def buildCandidate(phrase: Phrase,
                     parseResult: ParseResult,
                     value: Any,
                     params: Seq[Any]): Candidate =
    new Candidate(value,
      phrase.x,
      phrase.y,
      phrase.bold,
      phrase.height,
      phrase.pageNumber,
      getLocale,
      getType,
      buildProperties(phrase, parseResult, params))

  def buildProperties(phrase: Phrase, parseResult: ParseResult, params: Seq[Any]): Map[CandidateMetadata, Any] = Map.empty

}
