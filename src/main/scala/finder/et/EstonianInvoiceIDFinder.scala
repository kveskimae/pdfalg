package finder.et

import candidate.Candidate
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesRefreshedEvent
import regex.CommonRegex._

@Service
class EstonianInvoiceIDFinder extends AbstractFinder(null, null, true) {

  // TODO needs to listen context events together with other finders
  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = ("(?ism)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID)).r
    valuePattern = ("(?ism)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, INVOICE_ID) + "([.]{0,1})([\\s]{0,})([:]{0,1})([\\s]{0,})([^\\s]{1,})").r
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
    ret
  }

  override def isValueAllowed(value: Any): Boolean = {
    PATTERN_ESTONIAN_IBAN_AS_REGEX.findFirstIn(value.asInstanceOf[String]).isEmpty && PATTERN_AT_LEAST_2_INTEGER_NUMBERS_AS_REGEX.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  override def parseValue(raw: String): Any = StringUtils.normalizeSpace(raw)

  override def getType = INVOICE_ID

}
