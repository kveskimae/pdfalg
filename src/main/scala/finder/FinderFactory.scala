package finder

import java.io.InputStream
import java.util.Locale

import finder.et._
import finder.it._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.event.ContextRefreshedEvent
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{PDFFileParser, ParseResult}

@Service
class FinderFactory {

  // ESTONIA

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = _

  @Autowired var estonianInvoiceIDFinder: EstonianInvoiceIDFinder = _

  @Autowired var estonianNameFinder: EstonianNameFinder = _

  @Autowired var estonianReferenceNumberFinder: EstonianReferenceNumberFinder = _

  @Autowired var estonianTotalFinder: EstonianTotalFinder = _

  // ITALY

  @Autowired var italianInvoiceIDFinder: ItalianInvoiceIDFinder = _

  @Autowired var italianIssueDateFinder: ItalianIssueDateFinder = _

  @Autowired var italianNameFinder: ItalianNameFinder = _

  @Autowired var italianTotalBeforeTaxesFinder: ItalianTotalBeforeTaxesFinder = _

  @Autowired var italianTotalFinder: ItalianTotalFinder = _

  @Autowired var italianVATIdNumberFinder: ItalianVATIdNumberFinder = _

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

  private def makeFinder(lang: Locale, paymentFieldType: PaymentFieldType): AbstractFinder = {
    val langFinders: Option[Map[PaymentFieldType, AbstractFinder]] = finders.get(lang)
    if (langFinders.isEmpty) throw new IllegalArgumentException("Locale is not supported: " + lang)
    val ret: Option[AbstractFinder] = langFinders.get.get(paymentFieldType)
    if (ret.isEmpty) throw new IllegalArgumentException("Locale " + lang + " does not support payment field type " + paymentFieldType)
    ret.get
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

  def extractEstonian(pdfContentStream: InputStream): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    val finderResult = findCandidates(parseResult, SupportedLocales.ESTONIA, IBAN, INVOICE_ID, NAME, REFERENCE_NUMBER, TOTAL)
    finderResult
  }

  def extractItalian(pdfContentStream: InputStream): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    val finderResult = findCandidates(parseResult, SupportedLocales.ITALY, NAME, TOTAL, INVOICE_ID, ISSUE_DATE, VATIN, TOTAL_BEFORE_TAXES)
    finderResult
  }
}
