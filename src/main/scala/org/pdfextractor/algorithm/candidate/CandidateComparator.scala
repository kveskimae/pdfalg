package org.pdfextractor.algorithm

import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.SupportedLocales

package object candidate {

  val HEIGHT_FRACTION = 0.3
  val PAGE_FRACTION = 0.3
  val BOLD_FRACTION = 0.5
  val LOCATION_FRACTION = 0.5
  val PHRASE_TYPE_FRACTION = 0.5
  val PANK_FRACTION = 0.5
  val EURO_SIGN_FRACTION = 0.2
  val DOUBLE_NUMBER_FRACTION = 0.3
  val BELOW_FRACTION = 0.8
  val BIGGER_SUM_FRACTION = 0.8
  val NORMAL_TOTAL_LINE = 1.5

  // Assumes that payment field types match for the parameter candidates
  def compare(first: Candidate, other: Candidate): Int = {
    first.locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        first.paymentFieldType match {
          case REFERENCE_NUMBER =>
            // TODO
            return 0
            // return PositionalComparator.getInstance.compare(first, other)
          case IBAN =>
            return first.value.asInstanceOf[String].compareTo(other.value.asInstanceOf[String])
          case _ =>
        }
      case SupportedLocales.ITALIAN_LANG_CODE =>
        first.paymentFieldType match {
          case ISSUE_DATE =>
            return 0
          case VATIN =>
            return first.value.asInstanceOf[String].compareTo(other.value.asInstanceOf[String])
          case _ =>
        }
    }
    var thisLikelyhood = 0.0
    var otherLikelyhood = 0.0
    thisLikelyhood += first.height * HEIGHT_FRACTION
    otherLikelyhood += other.height * HEIGHT_FRACTION
    if (first.bold) thisLikelyhood += BOLD_FRACTION
    if (other.bold) otherLikelyhood += BOLD_FRACTION
    first.locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        first.paymentFieldType match {
          case TOTAL =>
            thisLikelyhood += calculateEstonianTotalPhraseTypeComparisonPart(first)
            otherLikelyhood += calculateEstonianTotalPhraseTypeComparisonPart(other)
          case NAME =>
            thisLikelyhood += calculateEstonianNameComparisonSum(first)
            otherLikelyhood += calculateEstonianNameComparisonSum(other)
          case _ =>
        }
      case SupportedLocales.ITALIAN_LANG_CODE =>
        first.paymentFieldType match {
          case TOTAL =>
          case TOTAL_BEFORE_TAXES =>
            thisLikelyhood += calculateItalianTotalPhraseTypeComparisonPart(first)
            otherLikelyhood += calculateItalianTotalPhraseTypeComparisonPart(other)
          case NAME =>
            thisLikelyhood += calculateItalianNamePhraseTypeComparisonPart(first)
            otherLikelyhood += calculateItalianNamePhraseTypeComparisonPart(other)
          case _ =>
        }
    }
    first.paymentFieldType match {
      case TOTAL =>
      case TOTAL_BEFORE_TAXES =>
        thisLikelyhood += calculateTotalComparisonSum(first)
        otherLikelyhood += calculateTotalComparisonSum(other)
        if ((first.y - other.y) > 10) thisLikelyhood += BELOW_FRACTION
        else if ((other.y - first.y) > 10) otherLikelyhood += BELOW_FRACTION
        if (first.value.asInstanceOf[Double] > other.value.asInstanceOf[Double]) thisLikelyhood += BIGGER_SUM_FRACTION
        else if (other.value.asInstanceOf[Double] > first.value.asInstanceOf[Double]) otherLikelyhood += BIGGER_SUM_FRACTION
      // TODO needs to fall through
      case NAME =>
      case INVOICE_ID =>
        if (first.pageNo < other.pageNo) thisLikelyhood += PAGE_FRACTION
        else if (other.pageNo < first.pageNo) otherLikelyhood += PAGE_FRACTION
        else {
          var locationDiff = 0
          if (isItalianNameCandidate(first)) locationDiff = first.y - other.y
          else if (isItalianInvoiceIdCandidate(first)) locationDiff = first.y - other.y
          // TODO else locationDiff = PositionalComparator.getInstance.compare(first, other)
          if (isItalianInvoiceIdCandidate(first)) { // Just prefer the one above
            if (locationDiff > 0) otherLikelyhood += 8 * LOCATION_FRACTION
            else if (locationDiff < 0) thisLikelyhood += 8 * LOCATION_FRACTION
          }
          else if (locationDiff > 0) {
            otherLikelyhood += LOCATION_FRACTION
            if (isEstonianNameCandidate(other)) otherLikelyhood += 0.1
          }
          else if (locationDiff < 0) {
            thisLikelyhood += LOCATION_FRACTION
            if (isEstonianNameCandidate(first)) thisLikelyhood += 0.1
          }
        }
      case _ =>
    }
    var ret = 0
    if (thisLikelyhood > otherLikelyhood) ret = -1
    else if (thisLikelyhood < otherLikelyhood) ret = 1
    ret
  }

  private def calculateItalianNamePhraseTypeComparisonPart(candidate: Candidate) = {
    var ret = 0.0
    val `type`: PhraseType = candidate.properties.get(PHRASE_TYPE).get.asInstanceOf[PhraseType]
    ret += `type`.getComparisonPart * PHRASE_TYPE_FRACTION
    ret
  }

  private def calculateItalianTotalPhraseTypeComparisonPart(candidate: Candidate) = {
    var ret = 0.0
    val `type`: PhraseType = candidate.properties.get(PHRASE_TYPE).get.asInstanceOf[PhraseType]
    ret += `type`.getComparisonPart * PHRASE_TYPE_FRACTION
    ret
  }

  private def calculateEstonianNameComparisonSum(candidate: Candidate) = {
    var ret = 0.0
    val `type`: PhraseType = candidate.properties.get(PHRASE_TYPE).get.asInstanceOf[PhraseType]
    ret += `type`.getComparisonPart * PHRASE_TYPE_FRACTION
    if (candidate.bold) ret += -0.3
    val isPank1 = candidate.properties.get(ESTONIAN_IS_PANK_PRESENT).asInstanceOf[Boolean]
    if (!isPank1) ret += PANK_FRACTION
    ret
  }

  private def calculateEstonianTotalPhraseTypeComparisonPart(first: Candidate) = {
    var ret = 0.0
    val phraseTypeOption: Option[Any] = first.properties.get(PHRASE_TYPE)
    if (phraseTypeOption.isEmpty) throw new IllegalStateException("Expecting phrase type to be present")
    val `type`: PhraseType = phraseTypeOption.get.asInstanceOf[PhraseType]
    ret += `type`.getComparisonPart * PHRASE_TYPE_FRACTION
    ret
  }

  private def calculateTotalComparisonSum(candidate: Candidate) = {
    var ret = 0.0
    if (candidate.properties.get(NORMAL_LINE).get.asInstanceOf[Boolean]) ret += NORMAL_TOTAL_LINE
    if (candidate.properties.get(EURO_SIGN_FOUND).get.asInstanceOf[Boolean]) ret += EURO_SIGN_FRACTION
    if (candidate.properties.get(DOUBLE_NUMBER).get.asInstanceOf[Boolean]) ret += DOUBLE_NUMBER_FRACTION
    ret
  }

  private def isItalianInvoiceIdCandidate(candidate: Candidate) = SupportedLocales.ITALY.getLanguage.equals(candidate.locale.getLanguage) && (candidate.paymentFieldType match {case INVOICE_ID => true; case _ => false})

  private def isItalianNameCandidate(candidate: Candidate) = SupportedLocales.ITALY.getLanguage.equals(candidate.locale.getLanguage) && (candidate.paymentFieldType match {case NAME => true; case _ => false})

  private def isEstonianNameCandidate(candidate: Candidate) = SupportedLocales.ESTONIA.getLanguage.equals(candidate.locale.getLanguage) && (candidate.paymentFieldType match {case NAME => true; case _ => false})

}
