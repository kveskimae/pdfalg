package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent

import scala.util.matching.Regex

@Service
class ItalianInvoiceIDFinder extends AbstractFinder {

  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX: Regex = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX: Regex = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX: Regex = null
  private[it] var PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX: Regex = null

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + "(.*)$").r)
    valuePattern = Some(("(?i)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + """([\s]{0,})([:]{0,1})([\s]{0,})(\w{1,}[^\s]{0,})""").r)
    PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX = ("(?i)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, INVOICE_ID) + """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX = ("^(?is)(?i)" + ITALIAN_INVOICE_ID_WORDS + "(.*)$").r
    PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX = ("(?i)" + ITALIAN_INVOICE_ID_WORDS + """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX = ("(?i)" + ITALIAN_INVOICE_ID_WORDS + """([\s]{0,})([:]{0,1})([\s]{0,})(\w{1,}[^\s]{0,})""").r
  }

  override def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    var ret: Seq[Candidate] = searchWithPattern(parseResult, getSearchPattern, getValuePattern)
    if (ret.isEmpty) {
      ret = searchWithPattern(parseResult, PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX, PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX)
    }
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val ret: Candidate = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, INVOICE_ID, Map.empty)
    ret
  }

  override def isValueAllowed(value: Any): Boolean = {
    PATTERN_INTEGER_NUMBER_AS_REGEX.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  override def parseValue(raw: String): Any = {
    StringUtils.normalizeSpace(

      PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX.replaceFirstIn(

        PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.replaceFirstIn(raw, ""),
        ""

      )

    )
  }

  override def parseValue(raw: String, regex: Regex): Any = {
    var ret: String = null
    if (PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.findFirstIn(raw).nonEmpty) ret =PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.replaceFirstIn(raw, "")
    else if (PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX.findFirstIn(raw).nonEmpty) ret =PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX.replaceFirstIn(raw, "")
    else throw new IllegalArgumentException("No invoice id start was found: '" + raw + "'")
    ret = StringUtils.normalizeSpace(ret)
    ret
  }

  override def getType = INVOICE_ID

}
