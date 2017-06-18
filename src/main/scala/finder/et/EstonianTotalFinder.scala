package finder.et

import java.util
import java.util.regex.Pattern

import candidate.{Candidate, PhraseType}
import dictionary._
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.CommonRegexPatterns._
import regex.RegexUtils

object EstonianTotalFinder {

  def countDotsAndCommas(number: String): Int = {
    val replaced = number.replaceAll(",", ".")
    val dotCount = StringUtils.countMatches(replaced, ".")
    dotCount
  }

  def isEuroPresent(text: String): Boolean = {
    val ret = RegexUtils.patternExistsInText(text, PATTERN_EURO_SIGN)
    ret
  }

  def isNormalTotalLine(text: String) = {
    val ret = RegexUtils.patternExistsInText(text, PATTERN_ESTONIAN_ORDINARY_TOTAL_LINE)
    ret
  }

}

class EstonianTotalFinder(override val phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, true) {

  def refreshed(): Unit = {
    searchPattern = Pattern.compile("^(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, TOTAL) + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    valuePattern = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS
  }

  protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Nothing): util.List[Candidate] = {
    val ret = new util.ArrayList[Candidate]
    val doubleValues = RegexUtils.searchForEstonianDoubleValuesAfterText(phrase.text)
    if (doubleValues.size == 1) {
      val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS.matcher(phrase.text)
      while ( {
        totalAsNumberMatcher.find
      }) {
        var totalAsString = totalAsNumberMatcher.group
        val dotCount = EstonianTotalFinder.countDotsAndCommas(totalAsString)
        if (dotCount < 2) {
          totalAsString = totalAsString.replaceAll(",", ".")
          val doubleNumber = dotCount > 0
          val totalAsDouble = parseValue(totalAsString).asInstanceOf[Double]
          val `type` = phraseTypesStore.findType(SupportedLocales.ESTONIA, TOTAL, phrase.text)
          val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
          ret.add(candidate)
        }
      }
    }
    ret
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val doubleNumber = params(0).asInstanceOf[Boolean]
    val `type` = params(1).asInstanceOf[PhraseType]
    val euroSignFound = EstonianTotalFinder.isEuroPresent(phrase.text)
    val normalTotalLine = EstonianTotalFinder.isNormalTotalLine(phrase.text)
    val properties: Map[PropertyType, Any] = Map(DOUBLE_NUMBER -> doubleNumber, PHRASE_TYPE -> `type`, EURO_SIGN_FOUND -> euroSignFound, NORMAL_LINE -> normalTotalLine)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, TOTAL, properties)
    ret
  }

  def isValueAllowed(value: Any) = true

  def parseValue(raw: String): Any = raw.toDouble

  def getType = TOTAL

}
