package org.pdfextractor.algorithm.finder.it

import java.util.Locale

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.parser.ParseResult
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

import scala.util.matching.Regex

@Service
class ItalianInvoiceIDFinder extends AbstractFinder {

  var PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX: Regex = _
  var PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX: Regex = _
  var PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX: Regex = _
  var PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX: Regex = _

  override def getLocale: Locale = SupportedLocales.ITALY

  override def getType = INVOICE_ID

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY,
        INVOICE_ID) + "(.*)$").r)
    valuePattern = Some(
      ("(?i)" + phraseTypesStore.buildAllPhrases(
        SupportedLocales.ITALY,
        INVOICE_ID) +
        """([\s]{0,})([:]{0,1})([\s]{0,})(\w{1,}[^\s]{0,})""").r)
    PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX =
      ("(?i)" + phraseTypesStore.buildAllPhrases(
        SupportedLocales.ITALY,
        INVOICE_ID) +
        """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX =
      ("^(?is)(?i)" + ItInvoiceIDWord + "(.*)$").r
    PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX =
      ("(?i)" + ItInvoiceIDWord + """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX =
      ("(?i)" + ItInvoiceIDWord + """([\s]{0,})([:]{0,1})([\s]{0,})(\w{1,}[^\s]{0,})""").r
  }

  override def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    var ret: Seq[Candidate] =
      searchWithPattern(parseResult, searchPattern.get, getValuePattern)
    if (ret.isEmpty) {
      ret = searchWithPattern(parseResult,
        PATTERN_ITALIAN_INVOICE_ID_START_BARE_AS_REGEX,
        PATTERN_ITALIAN_INVOICE_ID_LINE_BARE_AS_REGEX)
    }
    ret
  }

  override def isValueAllowed(value: Any): Boolean = {
    DigitsR.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  override def parseValue(raw: String): Any = {
    StringUtils.normalizeSpace({
      if (PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.findFirstIn(raw).nonEmpty) {
        PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.replaceFirstIn(raw, "")
      } else if (PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX.findFirstIn(raw).nonEmpty) {
        PATTERN_ITALIAN_INVOICE_ID_START_BARE_START_PART_AS_REGEX.replaceFirstIn(raw, "")
      } else {
        throw new IllegalArgumentException("No invoice id start was found: '" + raw + "'")
      }
    })
  }

}
