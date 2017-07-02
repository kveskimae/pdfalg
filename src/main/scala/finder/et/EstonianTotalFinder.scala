package finder.et

import java.util
import java.util.regex.Pattern

import candidate.Candidate
import dictionary._
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.TOTAL
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.context.event.ContextRefreshedEvent
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.{PhraseTypesRefreshedEvent, PhraseTypesStore}
import regex.CommonRegexPatterns._
import regex.RegexUtils

import scala.collection.mutable.ListBuffer

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

@Service
class EstonianTotalFinder extends AbstractFinder(null, null, true) {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Pattern.compile("^(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, TOTAL) + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    valuePattern = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS
  }

  override protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Pattern): ListBuffer[Candidate] = {
    val ret = scala.collection.mutable.ListBuffer.empty[Candidate]
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
          ret += candidate
        }
      }
    }
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val doubleNumber = params(0).asInstanceOf[Boolean]
    val `type` = params(1).asInstanceOf[PhraseType]
    val euroSignFound = EstonianTotalFinder.isEuroPresent(phrase.text)
    val normalTotalLine = EstonianTotalFinder.isNormalTotalLine(phrase.text)
    val properties: Map[PropertyType, Any] = Map(DOUBLE_NUMBER -> doubleNumber, PHRASE_TYPE -> `type`, EURO_SIGN_FOUND -> euroSignFound, NORMAL_LINE -> normalTotalLine)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, TOTAL, properties)
    ret
  }

  override def isValueAllowed(value: Any) = true

  override def parseValue(raw: String): Any = raw.replace(',', '.').toDouble

  override def getType = TOTAL

}
