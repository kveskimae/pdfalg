package finder

import java.util.Locale

import finder.et._
import finder.it._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.event.ContextRefreshedEvent
import org.springframework.stereotype.Service
import parser.{PDFFileParser, ParseResult}

@Service
class FinderFactory {

  // ESTONIA

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = null

  @Autowired var estonianInvoiceIDFinder: EstonianInvoiceIDFinder = null

  @Autowired var estonianNameFinder: EstonianNameFinder = null

  @Autowired var estonianReferenceNumberFinder: EstonianReferenceNumberFinder = null

  @Autowired var estonianTotalFinder: EstonianTotalFinder = null

  // ITALY

  @Autowired var italianInvoiceIDFinder: ItalianInvoiceIDFinder = null

  @Autowired var italianIssueDateFinder: ItalianIssueDateFinder = null

  @Autowired var italianNameFinder: ItalianNameFinder = null

  @Autowired var italianTotalBeforeTaxesFinder: ItalianTotalBeforeTaxesFinder = null

  @Autowired var italianTotalFinder: ItalianTotalFinder = null

  @Autowired var italianVATIdNumberFinder: ItalianVATIdNumberFinder = null

  var finders: Map[Locale, Map[PaymentFieldType, AbstractFinder]] = Map.empty

  @org.springframework.context.event.EventListener(Array(classOf[ContextRefreshedEvent]))
  def refreshed(): Unit = {
    if (finders.isEmpty) {
      val estonianFinders: Map[PaymentFieldType, AbstractFinder] =
        Map(
          INVOICE_ID -> estonianInvoiceIDFinder,
          IBAN -> estonianAccountNumberFinder,
          NAME -> estonianNameFinder,
          REFERENCE_NUMBER -> estonianReferenceNumberFinder,
          TOTAL -> estonianTotalFinder
        )

      val italianFinders: Map[PaymentFieldType, AbstractFinder] =
        Map(
          INVOICE_ID -> italianInvoiceIDFinder,
          NAME -> italianNameFinder,
          TOTAL -> italianTotalFinder,
          TOTAL_BEFORE_TAXES -> italianTotalBeforeTaxesFinder,
          ISSUE_DATE -> italianIssueDateFinder,
          VATIN -> italianVATIdNumberFinder
        )

      finders = Map(SupportedLocales.ESTONIA -> estonianFinders, SupportedLocales.ITALY -> italianFinders)
    }
  }

  private def makeFinder(lang: Locale, paymentFieldType: PaymentFieldType) = {
    val langFinders = finders.get(lang)
    val ret = langFinders.get(paymentFieldType)
    if (ret == null) throw new IllegalArgumentException("Locale " + lang + " does not support payment field type " + paymentFieldType)
    ret
  }

  private def findCandidates(parseResult: ParseResult, lang: Locale, fieldTypes: PaymentFieldType*) = {
    val ret = new FinderResult
    for (fieldType <- fieldTypes) {
      val abstractFinder: AbstractFinder = makeFinder(lang, fieldType)
      val candidates = abstractFinder.findCandidates(parseResult)
      val foundCandidates = ret.getCandidates(fieldType)
      foundCandidates ++= candidates
    }
    ret
  }

  def extractEstonian(pdfContentStream: Nothing): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    val finderResult = findCandidates(parseResult, SupportedLocales.ESTONIA, IBAN, INVOICE_ID, NAME, REFERENCE_NUMBER, TOTAL)
    finderResult
  }

  def extractItalian(pdfContentStream: Nothing): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    val finderResult = findCandidates(parseResult, SupportedLocales.ITALY, NAME, TOTAL, INVOICE_ID, ISSUE_DATE, VATIN, TOTAL_BEFORE_TAXES)
    finderResult
  }
}
