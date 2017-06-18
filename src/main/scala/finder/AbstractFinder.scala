package finder

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.regex.Pattern

import candidate.Candidate
import dictionary.{INVOICE_ID, PaymentFieldType, TOTAL}
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.{CommonRegexPatterns, RegexUtils}

import scala.collection.mutable.ListBuffer

import candidate.Candidate
import parser.ParseResult

object AbstractFinder {
  private val FOCUS_TYPE = INVOICE_ID // if you have a trouble with particular field type, set it here & let it log
  val log: Logger = LoggerFactory.getLogger(classOf[AbstractFinder])

  def isVoidPhrase(phrase: Phrase): Boolean = {
    if (phrase.text != null) return isVoidText(phrase.text)
    true
  }

  def isVoidText(text: String): Boolean = RegexUtils.patternMatches(text, CommonRegexPatterns.PATTERN_VOID)

  def combinePhrases(phrase: Phrase, otherPhrase: Phrase): Phrase = {
    val ret = new Phrase(otherPhrase.x, otherPhrase.y, otherPhrase.pageNumber, otherPhrase.height, phrase.width, phrase.text + " " + otherPhrase.text, otherPhrase.bold)
    ret
  }
}

abstract class AbstractFinder(val phraseTypesStore: PhraseTypesStore, var searchPattern: Pattern, var valuePattern: Pattern, val combinePhrases: Boolean, val combineFuzzy: Boolean = false) {

  def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    searchWithPattern(parseResult, searchPattern, valuePattern)
  }

  protected def searchWithPattern(parseResult: ParseResult, search: Pattern, value: Pattern): Seq[Candidate] = {
    val ret = collection.mutable.ListBuffer.empty[Candidate]
    if (AbstractFinder.log.isDebugEnabled && AbstractFinder.FOCUS_TYPE.equals(getType)) AbstractFinder.log.debug("Search pattern: " + search)
    for (phrase <- parseResult.phrases) {
      if (RegexUtils.patternExistsInText(phrase.text, search)) {
        if (AbstractFinder.log.isDebugEnabled && AbstractFinder.FOCUS_TYPE.equals(getType)) AbstractFinder.log.debug("Potential match: " + phrase.text)
        val foundValues = searchValuesFromPhrase(phrase, parseResult, value)
        if (combinePhrases) {
          if (foundValues.isEmpty) {
            var closestPhraseOnRight = parseResult.findClosestPhraseOnRight(phrase)
            while ( {
              closestPhraseOnRight != null
            }) if (AbstractFinder.isVoidPhrase(closestPhraseOnRight)) closestPhraseOnRight = parseResult.findClosestPhraseOnRight(closestPhraseOnRight)
            else {
              val combined = AbstractFinder.combinePhrases(phrase, closestPhraseOnRight)
              if (AbstractFinder.log.isDebugEnabled) {
                AbstractFinder.log.debug("combined with right: '" + combined + "'")
                AbstractFinder.log.debug("text on closestPhraseOnRight: '" + closestPhraseOnRight.text + "'")
              }
              val resultsFromCombined = searchValuesFromPhrase(combined, parseResult, value)
              addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
              closestPhraseOnRight = null
            }
          }
          if (foundValues.isEmpty) {
            val closestPhraseBelow = parseResult.findClosestPhraseBelow(phrase)
            if (closestPhraseBelow != null) {
              val combined = AbstractFinder.combinePhrases(phrase, closestPhraseBelow)
              if (AbstractFinder.log.isDebugEnabled) AbstractFinder.log.debug("combined with below: " + combined)
              val resultsFromCombined = searchValuesFromPhrase(combined, parseResult, value)
              if (TOTAL.equals(getType) && resultsFromCombined.isEmpty) {
                val closestPhraseBelowBelow = parseResult.findClosestPhraseBelow(closestPhraseBelow)
                if (closestPhraseBelowBelow != null) {
                  val combinedCombined = AbstractFinder.combinePhrases(combined, closestPhraseBelowBelow)
                  if (AbstractFinder.log.isDebugEnabled) AbstractFinder.log.debug("combinedCombined: " + combinedCombined)
                  val resultsFromCombinedCombined = searchValuesFromPhrase(combinedCombined, parseResult, value)
                  if (resultsFromCombinedCombined.isEmpty) if (AbstractFinder.log.isDebugEnabled) AbstractFinder.log.debug("empty")
                  else {
                    if (AbstractFinder.log.isDebugEnabled) AbstractFinder.log.debug("foundValues: " + resultsFromCombinedCombined)
                    addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombinedCombined)
                  }
                }
              }
              else addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
            }
          }
          if (combineFuzzy && foundValues.isEmpty) {
            val closestPhrasesBelowOrRight = parseResult.findClosestPhrasesBelowOrRight(phrase)
            for (closest <- closestPhrasesBelowOrRight) {
              val combined = AbstractFinder.combinePhrases(phrase, closest)
              if (AbstractFinder.log.isDebugEnabled) AbstractFinder.log.debug("combined with beloworright: " + combined)
              val resultsFromCombined = searchValuesFromPhrase(combined, parseResult, value)
              addElementsToAnotherListIfNotAlreadyContained(foundValues, resultsFromCombined)
            }
          }
        }
        addElementsToAnotherListIfNotAlreadyContained(ret, foundValues)
      }
    }
    ret.toList
  }

  protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Pattern): ListBuffer[Candidate] = {
    val ret = scala.collection.mutable.ListBuffer.empty[Candidate]
    val potentialValues: Seq[String] = RegexUtils.findMatches(phrase.text, valuePattern2)
    if (AbstractFinder.log.isDebugEnabled && AbstractFinder.FOCUS_TYPE.equals(getType)) AbstractFinder.log.debug("Searching values from phrase: " + phrase.text)
    for (potentialValue <- potentialValues) {
      val value = parseValue(potentialValue, valuePattern2)
      if (AbstractFinder.log.isDebugEnabled && AbstractFinder.FOCUS_TYPE.equals(getType)) AbstractFinder.log.debug("Potential value: " + value)
      if (isValueAllowed(value)) {
        if (AbstractFinder.log.isDebugEnabled && AbstractFinder.FOCUS_TYPE.equals(getType)) AbstractFinder.log.debug("Allowed value: " + value)
        ret += buildCandidate(parseResult, phrase, value)
      }
    }
    ret
  }

  protected def addElementsToAnotherListIfNotAlreadyContained(oldList: collection.mutable.ListBuffer[Candidate], newValues: Seq[Candidate]): Unit = {
    for (newValue <- newValues) {
      addOneElementToListIfNotAlreadyContained(oldList, newValue)
    }
  }

  protected def addOneElementToListIfNotAlreadyContained(oldList: collection.mutable.ListBuffer[Candidate], newValue: Candidate): Unit = {
    if (!oldList.contains(newValue)) oldList += newValue
    else {
      var toBeReplaced: Candidate = null
      for (oldValue <- oldList) {
        if (oldValue.value.equals(newValue.value)) toBeReplaced = oldValue
      }
      if (toBeReplaced != null && toBeReplaced.compareTo(newValue) > 0) {
        oldList -= toBeReplaced
        oldList += newValue
      }
    }
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate

  def isValueAllowed(value: Any): Boolean

  def parseValue(raw: String, pattern: Pattern): Any = parseValue(raw)

  def parseValue(raw: String): Any

  def getType: PaymentFieldType

  def getSearchPattern: Pattern = searchPattern

  def getValuePattern: Pattern = valuePattern
}
