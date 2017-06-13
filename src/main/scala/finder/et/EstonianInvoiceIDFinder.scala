package finder.et

import java.util.regex.Pattern

import candidate.Candidate
import dictionary.{INVOICE_ID, SupportedLocales}
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.CommonRegexPatterns._
import regex.RegexUtils

class EstonianInvoiceIDFinder(override val phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, true) {

  // TODO needs to listen context events together with other finders
  def refreshed(): Unit = {
    searchPattern = Pattern.compile(phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID), Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    valuePattern = Pattern.compile(phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID) + "([.]{0,1})([\\s]{0,})([:]{0,1})([\\s]{0,})([^\\s]{1,})", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
    ret
  }

  def isValueAllowed(value: Any): Boolean = {
    if (RegexUtils.patternExistsInText(value.asInstanceOf[String], PATTERN_ESTONIAN_IBAN)) return false
    val ret = RegexUtils.patternExistsInText(value.asInstanceOf[String], PATTERN_AT_LEAST_2_INTEGER_NUMBERS)
    ret
  }

  def parseValue(raw: String): Any = RegexUtils.fixWhiteSpace(raw)

  def getType = INVOICE_ID

}
