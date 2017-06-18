package finder.et

import java.util.regex.Pattern

import candidate.Candidate
import dictionary._
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.RegexUtils

@Service
class EstonianNameFinder(override val phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, true) {

  def refreshed(): Unit = {
    searchPattern = Pattern.compile("^" + phraseTypesStore.buildAllStarts(SupportedLocales.ESTONIA, NAME) + "$", Pattern.MULTILINE)
    valuePattern = Pattern.compile(phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, NAME), Pattern.MULTILINE)
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type` = phraseTypesStore.findType(SupportedLocales.ESTONIA, NAME, phrase.text)
    val pankPresent = isPankPresent(phrase.text)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`, ESTONIAN_IS_PANK_PRESENT -> pankPresent)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, NAME, properties)
    ret
  }

  def isValueAllowed(value: Any) = true

  def parseValue(raw: String): Any = {
    if (raw == null) return null
    var ret = raw.replaceAll("(Registrikood)(.{0,})", "")
    ret = ret.split("[\\s]{3,}")(0)
    ret = RegexUtils.fixWhiteSpace(ret)
    ret
  }

  def isPankPresent(text: String): Boolean = {
    val ret = RegexUtils.patternExistsInText(text, PATTERN_ESTONIAN_PANK)
    ret
  }

  def getType = NAME

}
