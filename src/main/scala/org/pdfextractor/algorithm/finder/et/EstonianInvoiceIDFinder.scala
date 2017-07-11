package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent

@Service
class EstonianInvoiceIDFinder extends AbstractFinder {

  // TODO needs to listen context events together with other finders
  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("(?ism)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID)).r)
    valuePattern = Some(("(?ism)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID) + """([.]{0,1})([\s]{0,})([:]{0,1})([\s]{0,})([^\s]{1,})""").r)
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
  }

  override def isValueAllowed(value: Any): Boolean = {
    PATTERN_ESTONIAN_IBAN_AS_REGEX.findFirstIn(value.asInstanceOf[String]).isEmpty && PATTERN_AT_LEAST_2_INTEGER_NUMBERS_AS_REGEX.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  override def parseValue(raw: String): Any = StringUtils.normalizeSpace(raw)

  override def getType = INVOICE_ID

}
