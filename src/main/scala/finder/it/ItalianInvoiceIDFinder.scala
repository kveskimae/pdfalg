package finder.it

import java.util.regex.Pattern

import candidate.Candidate
import dictionary.SupportedLocales
import finder.AbstractFinder
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.CommonRegexPatterns._
import regex.RegexUtils

@Service
class ItalianInvoiceIDFinder(phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, true) {

  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_PART: Pattern = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_BARE: Pattern = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART: Pattern = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_LINE_BARE: Pattern = null

  def refreshed(): Unit = {
    searchPattern = Pattern.compile("^" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    valuePattern = Pattern.compile(phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + "([\\s]{0,})([:]{0,1})([\\s]{0,})(\\w{1,}[^\\s]{0,})", Pattern.CASE_INSENSITIVE)
    PATTERN_ITALIAN_INVOICE_ID_START_PART = Pattern.compile(phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + "([\\s]{0,})([:]{0,1})([\\s]{0,})", Pattern.CASE_INSENSITIVE)
    PATTERN_ITALIAN_INVOICE_ID_START_BARE = Pattern.compile("^" + ITALIAN_INVOICE_ID_WORDS + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
    PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART = Pattern.compile(ITALIAN_INVOICE_ID_WORDS + "([\\s]{0,})([:]{0,1})([\\s]{0,})", Pattern.CASE_INSENSITIVE)
    PATTERN_ITALIAN_INVOICE_ID_LINE_BARE = Pattern.compile(ITALIAN_INVOICE_ID_WORDS + "([\\s]{0,})([:]{0,1})([\\s]{0,})(\\w{1,}[^\\s]{0,})", Pattern.CASE_INSENSITIVE)
  }

  def findCandidates(parseResult: Nothing): Seq[Candidate] = {
    var ret: Seq[Candidate] = searchWithPattern(parseResult, searchPattern, valuePattern)
    if (ret.isEmpty) {
      ret = searchWithPattern(parseResult, PATTERN_ITALIAN_INVOICE_ID_START_BARE, PATTERN_ITALIAN_INVOICE_ID_LINE_BARE)
    }
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val ret: Candidate = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, INVOICE_ID, Map.empty)
    ret
  }

  override def isValueAllowed(value: Any): Boolean = {
    val ret: Boolean = RegexUtils.patternExistsInText(value.asInstanceOf[String], PATTERN_INTEGER_NUMBER)
    ret
  }

  override def parseValue(raw: String): Any = {
    var ret: String = RegexUtils.removeFirstOccurrence(raw, PATTERN_ITALIAN_INVOICE_ID_START_PART)
    if (ret.length == raw.length) ret = RegexUtils.removeFirstOccurrence(raw, PATTERN_ITALIAN_INVOICE_ID_START_BARE)
    ret = RegexUtils.fixWhiteSpace(ret)
    ret
  }

  override def parseValue(raw: String, pattern: Pattern): Any = {
    var ret: String = null
    if (RegexUtils.patternExistsInText(raw, PATTERN_ITALIAN_INVOICE_ID_START_PART)) ret = RegexUtils.removeFirstOccurrence(raw, PATTERN_ITALIAN_INVOICE_ID_START_PART)
    else if (RegexUtils.patternExistsInText(raw, PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART)) ret = RegexUtils.removeFirstOccurrence(raw, PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART)
    else throw new IllegalArgumentException("No invoice id start was found: '" + raw + "'")
    ret = RegexUtils.fixWhiteSpace(ret)
    ret
  }

  override def getType = INVOICE_ID

}
