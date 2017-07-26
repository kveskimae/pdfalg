package org.pdfextractor.algorithm.candidate

import java.util.{Locale, Objects}

import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.beans.BeanProperty

/**
  * Natural orderings are not consistent with <i>equals</i>.
  * <br />
  * Arranges sorted collection of candidates by likelyhood
  */
case class Candidate(@BeanProperty // for dependent RESTful API in Java
                     value: Any,
                     x: Integer,
                     y: Integer,
                     bold: Boolean,
                     height: Integer,
                     pageNo: Integer,
                     locale: Locale,
                     paymentFieldType: PaymentFieldType,
                     properties: Map[CandidateMetadata, Any])
    extends Comparable[Candidate] {

  Objects.requireNonNull(value)

  override def compareTo(other: Candidate): Int = compare(this, other)

  override def equals(other: Any): Boolean = {
    other match {
      case that: Candidate => this.value == that.value
      case _               => false
    }
  }

  override def hashCode(): Int = value.hashCode()

}
