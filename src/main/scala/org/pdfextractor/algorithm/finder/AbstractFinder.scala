package org.pdfextractor.algorithm.finder

import java.util.Objects

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesStore

import scala.collection.mutable
import scala.util.matching.Regex

abstract class AbstractFinder(var searchPattern: Option[Regex],
                              var valuePattern: Option[Regex],
                              val combinePhrases: Boolean = true,
                              val combineFuzzy: Boolean = false) {

  val log: Logger = LoggerFactory.getLogger(classOf[AbstractFinder])

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

  @Autowired var phraseTypesStore: PhraseTypesStore = _

  def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    searchWithPattern(parseResult, getSearchPattern, getValuePattern)
  }

  def tryCombiningWithRight(phrase: Phrase,
                            foundValues: mutable.Buffer[Candidate],
                            parseResult: ParseResult,
                            value: Regex) = {

    def searchRecursively(cur: Option[Phrase]): Option[Phrase] = {
      if (cur.isEmpty) {
        None
      } else if (isVoidPhrase(cur.get)) {
        searchRecursively(parseResult.findClosestPhraseOnRight(cur.get))
      } else {
        cur
      }
    }

    val closestPhraseOnRight: Option[Phrase] =
      searchRecursively(parseResult.findClosestPhraseOnRight(phrase))

    if (closestPhraseOnRight.isDefined) {
      val combined = combinePhrases1(phrase, closestPhraseOnRight.get)
      val resultsFromCombined =
        searchValuesFromPhrase(combined, parseResult, value)
      addElementsToAnotherListIfNotAlreadyContained(foundValues,
        resultsFromCombined)
    }
  }

  def tryCombiningWithBelow(phrase: Phrase,
                            foundValues: mutable.Buffer[Candidate],
                            parseResult: ParseResult,
                            value: Regex) = {

    parseResult.findClosestPhraseBelow(phrase) match {

      case Some(closestPhraseBelow) => {

        val combined = combinePhrases1(phrase, closestPhraseBelow)

        searchValuesFromPhrase(combined, parseResult, value) match {

          case resultsFromCombined if resultsFromCombined.isEmpty && TOTAL == getType => {
            val closestPhraseBelowBelow: Option[Phrase] = parseResult.findClosestPhraseBelow(closestPhraseBelow)
            if (closestPhraseBelowBelow.isDefined) {
              val combinedCombined = combinePhrases1(combined, closestPhraseBelowBelow.get)
              val resultsFromCombinedCombined = searchValuesFromPhrase(combinedCombined, parseResult, value)
              if (!resultsFromCombinedCombined.isEmpty) {
                addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombinedCombined)
              }
            }
          }

          case resultsFromCombined => addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
        }
      }

      case None =>
    }
  }

  def tryCombiningFuzzy(phrase: Phrase,
                        foundValues: mutable.Buffer[Candidate],
                        parseResult: ParseResult,
                        value: Regex) = {
    parseResult
      .findClosestPhrasesBelowOrRight(phrase)
      .map(closest => combinePhrases1(phrase, closest))
      .map(combined => searchValuesFromPhrase(combined, parseResult, value))
      .foreach(
        candidate => {
          addElementsToAnotherListIfNotAlreadyContained(foundValues, candidate)
        }
      )
  }

  protected def searchWithPattern(parseResult: ParseResult,
                                  search: Regex,
                                  value: Regex): Seq[Candidate] = {
    Objects.requireNonNull(parseResult)

    parseResult.phrases
      .filter({ (phrase: Phrase) =>
        search.findFirstIn(phrase.text).nonEmpty
      })
      .map(
        (phrase: Phrase) => {
          val foundValues: mutable.Buffer[Candidate] =
            searchValuesFromPhrase(phrase, parseResult, value)

          if (combinePhrases) {
            if (foundValues.isEmpty) {
              // try together with phrase on right
              tryCombiningWithRight(phrase, foundValues, parseResult, value)
            }
            if (foundValues.isEmpty) {
              // no success in combining with right, try below
              tryCombiningWithBelow(phrase, foundValues, parseResult, value)
            }
            if (foundValues.isEmpty && combineFuzzy) {
              // no success with below either, loosen search up
              tryCombiningFuzzy(phrase, foundValues, parseResult, value)
            }
          }

          foundValues
        }
      ).flatMap(_.iterator).sorted
  }

  protected def searchValuesFromPhrase(
                                        phrase: Phrase,
                                        parseResult: ParseResult,
                                        valuePattern2: Regex): mutable.Buffer[Candidate] = {
    valuePattern2
      .findAllIn(phrase.text)
      .map(parseValue(_, valuePattern2))
      .filter(isValueAllowed(_))
      .map(buildCandidate(parseResult, phrase, _))
      .toBuffer
  }

  protected def addElementsToAnotherListIfNotAlreadyContained(
                                                               oldList: mutable.Buffer[Candidate],
                                                               newValues: Seq[Candidate]): Unit = {
    newValues.foreach(addOneElementToListIfNotAlreadyContained(oldList, _))
  }

  protected def addOneElementToListIfNotAlreadyContained(
                                                          oldList: mutable.Buffer[Candidate],
                                                          newValue: Candidate): Unit = {
    if (!oldList.contains(newValue)) oldList += newValue
    else {
      var toBeReplaced: Option[Candidate] = None
      for (oldValue <- oldList) {
        if (oldValue.value.equals(newValue.value)) toBeReplaced = Some(oldValue)
      }
      if (toBeReplaced.isDefined && toBeReplaced.get.compareTo(newValue) > 0) {
        oldList -= toBeReplaced.get
        oldList += newValue
      }
    }
  }

  protected def buildCandidate(parseResult: ParseResult,
                               phrase: Phrase,
                               value: Any,
                               params: Any*): Candidate

  def isValueAllowed(value: Any): Boolean

  def parseValue(raw: String, pattern: Regex): Any = parseValue(raw)

  def parseValue(raw: String): Any

  def getType: PaymentFieldType

  def getSearchPattern: Regex = searchPattern.get

  def getValuePattern: Regex = valuePattern.get
}
