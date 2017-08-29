package org.pdfextractor.algorithm.candidate

import java.util.Locale
import java.util.Objects._

import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.beans.BeanProperty

/**
  * Natural orderings are not consistent with <i>equals</i>.
  * <br />
  * Arranges sorted collection of candidates by likelyhood
  */
case class Candidate(@BeanProperty // for dependent RESTful API in Java
                     value: Any,
                     locale: Locale,
                     paymentFieldType: PaymentFieldType,
                     features: Map[CandidateFeatureType, Any])
  extends Comparable[Candidate] {

  requireNonNull(value)

  override def compareTo(other: Candidate): Int = compare(this, other)

  override def equals(other: Any): Boolean = {
    other match {
      case that: Candidate => this.value == that.value
      case _ => false
    }
  }

  override def hashCode(): Int = value.hashCode()

  def x: Integer = features.getOrElse(X, 1).asInstanceOf[Integer]
  def y: Integer = features.getOrElse(Y, 1).asInstanceOf[Integer]
  def bold: Boolean = features.getOrElse(Bold, false).asInstanceOf[Boolean]
  def height: Integer = features.getOrElse(Height, 1).asInstanceOf[Integer]
  def pageNo: Integer = features.getOrElse(PageNo, 1).asInstanceOf[Integer]

}
