package org.pdfextractor.algorithm.finder

import java.io.InputStream
import java.util.Locale

import org.pdfextractor.algorithm.finder.et._
import org.pdfextractor.algorithm.finder.it._
import org.pdfextractor.algorithm.parser.{PDFFileParser, ParseResult}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

@Service
class FinderFactory {

  // ESTONIA

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = _

  @Autowired var estonianInvoiceIDFinder: EstonianInvoiceIDFinder = _

  @Autowired var estonianNameFinder: EstonianNameFinder = _

  @Autowired var estonianReferenceNumberFinder: EstonianReferenceNumberFinder =
    _

  @Autowired var estonianTotalFinder: EstonianTotalFinder = _

  // ITALY

  @Autowired var italianInvoiceIDFinder: ItalianInvoiceIDFinder = _

  @Autowired var italianIssueDateFinder: ItalianIssueDateFinder = _

  @Autowired var italianNameFinder: ItalianNameFinder = _

  @Autowired var italianTotalBeforeTaxesFinder: ItalianTotalBeforeTaxesFinder =
    _

  @Autowired var italianTotalFinder: ItalianTotalFinder = _

  @Autowired var italianVATIdNumberFinder: ItalianVATIdNumberFinder = _

  def extractEstonian(pdfContentStream: InputStream): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    findCandidates(parseResult,
      SupportedLocales.ESTONIA,
      IBAN,
      INVOICE_ID,
      NAME,
      REFERENCE_NUMBER,
      TOTAL)
  }

  def extractItalian(pdfContentStream: InputStream): FinderResult = {
    val parseResult = PDFFileParser.parse(pdfContentStream)
    findCandidates(parseResult,
      SupportedLocales.ITALY,
      NAME,
      TOTAL,
      INVOICE_ID,
      ISSUE_DATE,
      VATIN,
      TOTAL_BEFORE_TAXES)
  }

  private def findCandidates(parseResult: ParseResult,
                             lang: Locale,
                             fieldTypes: PaymentFieldType*) = {
    val ret = new FinderResult
    fieldTypes.foreach(fieldType => {
      val abstractFinder: Option[AbstractFinder] = makeFinder(lang, fieldType)
      abstractFinder match {
        case Some(finder) =>
          ret.getCandidates(fieldType) ++= finder.findCandidates(parseResult)
        case None =>
      }
    })
    ret
  }

  private def makeFinder(
                          lang: Locale,
                          paymentFieldType: PaymentFieldType): Option[AbstractFinder] = {
    lang.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        makeEstonianFinder(paymentFieldType)
      case SupportedLocales.ITALIAN_LANG_CODE =>
        makeItalianFinder(paymentFieldType)
      case _ => None
    }
  }

  def makeEstonianFinder(
                          paymentFieldType: PaymentFieldType): Option[AbstractFinder] =
    paymentFieldType match {
      case INVOICE_ID => Some(estonianInvoiceIDFinder)
      case IBAN => Some(estonianAccountNumberFinder)
      case NAME => Some(estonianNameFinder)
      case REFERENCE_NUMBER => Some(estonianReferenceNumberFinder)
      case TOTAL => Some(estonianTotalFinder)
      case _ => None
    }

  def makeItalianFinder(
                         paymentFieldType: PaymentFieldType): Option[AbstractFinder] =
    paymentFieldType match {
      case INVOICE_ID => Some(italianInvoiceIDFinder)
      case NAME => Some(italianNameFinder)
      case TOTAL => Some(italianTotalFinder)
      case TOTAL_BEFORE_TAXES => Some(italianTotalBeforeTaxesFinder)
      case ISSUE_DATE => Some(italianIssueDateFinder)
      case VATIN => Some(italianVATIdNumberFinder)
      case _ => None
    }
}
