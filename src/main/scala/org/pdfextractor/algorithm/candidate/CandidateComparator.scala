package org.pdfextractor.algorithm

import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.SupportedLocales

package object candidate {

  val HeightFraction = 0.3
  val PageFraction = 0.3
  val BoldFraction = 0.5
  val LocationFraction = 0.5
  val PhraseTypeFraction = 0.5
  val PankFraction = 0.5
  val EuroSignFraction = 0.2
  val DoubleFraction = 0.3
  val BelowFraction = 0.8
  val BiggerSumFraction = 0.8
  val NormalLineFraction = 1.5

  implicit def candidate2PhraseType(candidate: Candidate): PhraseType =
    candidate.properties.get(MetaPhraseType).get.asInstanceOf[PhraseType]

  // Assumes that payment field types match for the parameter candidates
  def compare(first: Candidate, other: Candidate): Int = {
    val fastResult: Option[Int] = tryFastResulting(first, other)

    fastResult match {
      case Some(ret) => ret
      case _ => calcCompareResult(first, other)
    }
  }

  def tryFastResulting(first: Candidate, other: Candidate): Option[Int] = {
    first.locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        first.paymentFieldType match {
          case REFERENCE_NUMBER => Some(0)
          case IBAN =>
            Some(
              first.value
                .asInstanceOf[String]
                .compareTo(other.value.asInstanceOf[String]))
          case _ => None
        }
      case SupportedLocales.ITALIAN_LANG_CODE =>
        first.paymentFieldType match {
          case ISSUE_DATE => Some(0)
          case VATIN =>
            Some(
              first.value
                .asInstanceOf[String]
                .compareTo(other.value.asInstanceOf[String]))
          case _ => None
        }
      case _ => None
    }
  }

  def calcCompareResult(first: Candidate, other: Candidate): Int = {
    var thisLikelyhood = 0.0
    var otherLikelyhood = 0.0
    thisLikelyhood += first.height * HeightFraction
    otherLikelyhood += other.height * HeightFraction
    if (first.bold) thisLikelyhood += BoldFraction
    if (other.bold) otherLikelyhood += BoldFraction
    first.locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        first.paymentFieldType match {
          case TOTAL =>
            thisLikelyhood += calcPhraseTypePart(first)
            otherLikelyhood += calcPhraseTypePart(other)
          case NAME =>
            thisLikelyhood += calculateEstonianNameComparisonSum(first)
            otherLikelyhood += calculateEstonianNameComparisonSum(other)
          case _ =>
        }
      case SupportedLocales.ITALIAN_LANG_CODE =>
        first.paymentFieldType match {
          case TOTAL | TOTAL_BEFORE_TAXES | NAME =>
            thisLikelyhood += calcPhraseTypePart(first)
            otherLikelyhood += calcPhraseTypePart(other)
          case _ =>
        }
    }
    first.paymentFieldType match {
      case TOTAL | TOTAL_BEFORE_TAXES =>
        thisLikelyhood += calculateTotalComparisonSum(first)
        otherLikelyhood += calculateTotalComparisonSum(other)

        if ((first.y - other.y) > 10) thisLikelyhood += BelowFraction
        else if ((other.y - first.y) > 10) otherLikelyhood += BelowFraction
        if (first.value.asInstanceOf[Double] > other.value.asInstanceOf[Double])
          thisLikelyhood += BiggerSumFraction
        else if (other.value.asInstanceOf[Double] > first.value
          .asInstanceOf[Double]) otherLikelyhood += BiggerSumFraction

      // TODO needs to fall through
      case NAME | INVOICE_ID =>
        (first.pageNo - other.pageNo) match {
          case diff if (diff < 0) => thisLikelyhood += PageFraction
          case diff if (diff > 0) => otherLikelyhood += PageFraction
          case _ => {
            var locationDiff = 0
            if (isItalianNameCandidate(first)) locationDiff = first.y - other.y
            else if (isItalianInvoiceIdCandidate(first))
              locationDiff = first.y - other.y
            // TODO else locationDiff = PositionalComparator.getInstance.compare(first, other)
            if (isItalianInvoiceIdCandidate(first)) { // Just prefer the one above
              if (locationDiff > 0) otherLikelyhood += 8 * LocationFraction
              else if (locationDiff < 0) thisLikelyhood += 8 * LocationFraction
            } else if (locationDiff > 0) {
              otherLikelyhood += LocationFraction
              if (isEstonianNameCandidate(other)) otherLikelyhood += 0.1
            } else if (locationDiff < 0) {
              thisLikelyhood += LocationFraction
              if (isEstonianNameCandidate(first)) thisLikelyhood += 0.1
            }
          }
        }
      case _ =>
    }

    (thisLikelyhood - otherLikelyhood) match {
      case diff if diff > 0 => -1
      case diff if diff < 0 => 1
      case _ => 0
    }
  }

  private def calcPhraseTypePart(phraseType: PhraseType) = {
    phraseType.getComparisonPart * PhraseTypeFraction
  }

  private def calculateEstonianNameComparisonSum(candidate: Candidate) = {
    var ret = 0.0
    val phraseType: PhraseType =
      candidate.properties.get(MetaPhraseType).get.asInstanceOf[PhraseType]
    ret += phraseType.getComparisonPart * PhraseTypeFraction
    if (candidate.bold) ret += -0.3
    val isPank1 = candidate.properties.get(HasPank).asInstanceOf[Boolean]
    if (!isPank1) ret += PankFraction
    ret
  }

  private def calculateTotalComparisonSum(candidate: Candidate) = {
    var ret = 0.0
    if (candidate.properties.get(IsNormalLine).get.asInstanceOf[Boolean])
      ret += NormalLineFraction
    if (candidate.properties.get(HasEuroSign).get.asInstanceOf[Boolean])
      ret += EuroSignFraction
    if (candidate.properties.get(IsDouble).get.asInstanceOf[Boolean])
      ret += DoubleFraction
    ret
  }

  private def isItalianInvoiceIdCandidate(candidate: Candidate) = {
    candidate.locale.getLanguage match {
      case SupportedLocales.ITALIAN_LANG_CODE =>
        candidate.paymentFieldType == INVOICE_ID
      case _ => false
    }
  }

  private def isItalianNameCandidate(candidate: Candidate) = {
    candidate.locale.getLanguage match {
      case SupportedLocales.ITALIAN_LANG_CODE =>
        candidate.paymentFieldType == NAME
      case _ => false
    }
  }

  private def isEstonianNameCandidate(candidate: Candidate) = {
    candidate.locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        candidate.paymentFieldType == NAME
      case _ => false
    }
  }

}
