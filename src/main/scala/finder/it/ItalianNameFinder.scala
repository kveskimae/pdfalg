package finder.it

import java.util.regex.Pattern

import candidate.Candidate
import dictionary._
import finder.AbstractFinder
import finder.it.ItalianRegexPatterns._
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.CommonRegexPatterns._
import regex.RegexUtils

object ItalianNameFinder {
  val MINIMUM_NUMBER_OF_CHARACTERS: Integer = 3

  private val PATTERN_MINIMUM_CHARACTERS: Pattern = Pattern.compile ("^.*" + ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND + "{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}.*$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
}

class ItalianNameFinder(phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, false) {

  def refreshed(): Unit = {
    searchPattern = Pattern.compile("^" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, NAME) + "$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    valuePattern = Pattern.compile("^(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type` = phraseTypesStore.findType(SupportedLocales.ITALY, NAME, phrase.text)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, NAME, properties)
    ret
  }

  def isValueAllowed(value: Any): Boolean = {
    val ret = !AbstractFinder.isVoidText(value.asInstanceOf[String]) && !RegexUtils.patternExistsInText(value.asInstanceOf[String], PATTERN_ITALIAN_NAME_FORBIDDEN) && RegexUtils.patternExistsInText(value.asInstanceOf[String], PATTERN_MINIMUM_CHARACTERS)
    ret
  }

  def parseValue(raw: String): Any = {
    if (raw == null) return null
    var ret = raw
    ret = RegexUtils.fixWhiteSpace(ret)
    val bits = ret.split(",")
    ret = bits(0)
    ret
  }

  def getType = NAME

}
