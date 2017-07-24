package org.pdfextractor.algorithm.finder

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesStore

import scala.collection.mutable
import scala.util.matching.Regex

abstract class AbstractFinder(var searchPattern: Option[Regex], var valuePattern: Option[Regex], val combinePhrases: Boolean = true, val combineFuzzy: Boolean = false) {

  val log: Logger = LoggerFactory.getLogger(classOf[AbstractFinder])

  def this() = this(None, None, true, false)
  def this(searchPattern2: Regex, valuePattern2: Regex) = this(Some(searchPattern2), Some(valuePattern2))
  def this(searchPattern2: Regex, valuePattern2: Regex, combinePhrases2: Boolean) = this(Some(searchPattern2), Some(valuePattern2), combinePhrases2)
  def this(searchPattern2: Regex, valuePattern2: Regex, combinePhrases2: Boolean, combineFuzzy2: Boolean) = this(Some(searchPattern2), Some(valuePattern2), combinePhrases2, combineFuzzy2)

  @Autowired var phraseTypesStore: PhraseTypesStore = _

  def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    searchWithPattern(parseResult, getSearchPattern, getValuePattern)
  }

  protected def searchWithPattern(parseResult: ParseResult, search: Regex, value: Regex): Seq[Candidate] = {
    Option(parseResult).orElse(throw new NullPointerException)
    val ret = collection.mutable.ListBuffer.empty[Candidate]
    if (log.isDebugEnabled && FOCUS_TYPE.equals(getType)) log.debug("Search pattern: " + search)
    for (phrase: Phrase <- parseResult.phrases) {
      if (search.findFirstIn(phrase.text).nonEmpty) {
        if (log.isDebugEnabled && FOCUS_TYPE.equals(getType)) log.debug("Potential match: " + phrase.text)
        val foundValues: mutable.Buffer[Candidate] = searchValuesFromPhrase(phrase, parseResult, value)
        if (combinePhrases) {
          if (foundValues.isEmpty) {
            var closestPhraseOnRight: Option[Phrase] = parseResult.findClosestPhraseOnRight(phrase)
            while ( {
              closestPhraseOnRight.isDefined
            }) if (isVoidPhrase(closestPhraseOnRight.get)) closestPhraseOnRight = parseResult.findClosestPhraseOnRight(closestPhraseOnRight.get)
            else {
              val combined = combinePhrases1(phrase, closestPhraseOnRight.get)
              if (log.isDebugEnabled) {
                log.debug("combined with right: '" + combined + "'")
                log.debug("text on closestPhraseOnRight: '" + closestPhraseOnRight.get.text + "'")
              }
              val resultsFromCombined = searchValuesFromPhrase(combined, parseResult, value)
              addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
              closestPhraseOnRight = None
            }
          }
          if (foundValues.isEmpty) {
            val closestPhraseBelow: Option[Phrase] = parseResult.findClosestPhraseBelow(phrase)
            if (closestPhraseBelow.isDefined) {
              val combined = combinePhrases1(phrase, closestPhraseBelow.get)
              if (log.isDebugEnabled) log.debug("combined with below: " + combined)
              val resultsFromCombined = searchValuesFromPhrase(combined, parseResult, value)
              if (TOTAL.equals(getType) && resultsFromCombined.isEmpty) {
                val closestPhraseBelowBelow: Option[Phrase] = parseResult.findClosestPhraseBelow(closestPhraseBelow.get)
                if (closestPhraseBelowBelow.isDefined) {
                  val combinedCombined = combinePhrases1(combined, closestPhraseBelowBelow.get)
                  if (log.isDebugEnabled) log.debug("combinedCombined: " + combinedCombined)
                  val resultsFromCombinedCombined = searchValuesFromPhrase(combinedCombined, parseResult, value)
                  if (resultsFromCombinedCombined.isEmpty) if (log.isDebugEnabled) log.debug("empty")
                  else {
                    if (log.isDebugEnabled) log.debug("foundValues: " + resultsFromCombinedCombined)
                    addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombinedCombined)
                  }
                }
              }
              else addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
            }
          }
          if (combineFuzzy && foundValues.isEmpty) {
            parseResult.
              findClosestPhrasesBelowOrRight(phrase).
              map(closest => combinePhrases1(phrase, closest)).
              map(combined => searchValuesFromPhrase(combined, parseResult, value)).
              foreach(
                candidate => {
                  addElementsToAnotherListIfNotAlreadyContained(foundValues, candidate)
                }
              )
          }
        }
        addElementsToAnotherListIfNotAlreadyContained(ret, foundValues)
      }
    }
    ret.toList.sorted
  }

  protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Regex): mutable.Buffer[Candidate] = {
    valuePattern2.
      findAllIn(phrase.text).
      map(parseValue(_, valuePattern2)).
      filter(isValueAllowed(_)).
      map(buildCandidate(parseResult, phrase, _)).
      toBuffer
  }

  protected def addElementsToAnotherListIfNotAlreadyContained(oldList: mutable.Buffer[Candidate], newValues: Seq[Candidate]): Unit = {
    newValues.foreach(addOneElementToListIfNotAlreadyContained(oldList, _))
  }

  protected def addOneElementToListIfNotAlreadyContained(oldList: mutable.Buffer[Candidate], newValue: Candidate): Unit = {
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

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate

  def isValueAllowed(value: Any): Boolean

  def parseValue(raw: String, pattern: Regex): Any = parseValue(raw)

  def parseValue(raw: String): Any

  def getType: PaymentFieldType

  def getSearchPattern: Regex = searchPattern.get

  def getValuePattern: Regex = valuePattern.get
}
