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
class ItalianInvoiceIDFinder extends AbstractFinder(SupportedLocales.ITALY, INVOICE_ID) {

  var StartR: Regex = _
  var BareStartR: Regex = _
  var BareStartStartPartR: Regex = _
  var BareLineR: Regex = _

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
    StartR =
      ("(?i)" + phraseTypesStore.buildAllPhrases(
        SupportedLocales.ITALY,
        INVOICE_ID) +
        """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    BareStartR =
      ("^(?is)(?i)" + ItInvoiceIDWord + "(.*)$").r
    BareStartStartPartR =
      ("(?i)" + ItInvoiceIDWord + """([\s]{0,})([:]{0,1})([\s]{0,})""").r
    BareLineR =
      ("(?i)" + ItInvoiceIDWord + """([\s]{0,})([:]{0,1})([\s]{0,})(\w{1,}[^\s]{0,})""").r
  }

  override def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    var ret: Seq[Candidate] =
      searchWithPattern(parseResult, searchPattern.get, getValuePattern)
    if (ret.isEmpty) {
      ret = searchWithPattern(parseResult,
        BareStartR,
        BareLineR)
    }
    ret
  }

  override def isValueAllowed(value: Any): Boolean = {
    DigitsR.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  override def parseValue(raw: String): Any = {
    StringUtils.normalizeSpace({
      if (StartR.findFirstIn(raw).nonEmpty) {
        StartR.replaceFirstIn(raw, "")
      } else if (BareStartStartPartR.findFirstIn(raw).nonEmpty) {
        BareStartStartPartR.replaceFirstIn(raw, "")
      } else {
        throw new IllegalArgumentException("No invoice id start was found: '" + raw + "'")
      }
    })
  }

}
