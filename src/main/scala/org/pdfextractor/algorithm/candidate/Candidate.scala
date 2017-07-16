package org.pdfextractor.algorithm.candidate

import java.util.Locale

import org.apache.commons.lang3.builder.{HashCodeBuilder, ReflectionToStringBuilder, ToStringStyle}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.beans.BeanProperty

/*
* Note: Natural orderings in subclass implementations of Comparable are not consistent with equals.
*
* Also rearranges more likely candidates to the start of the collection.
*/
case class Candidate(@BeanProperty val value: Any,
                val x: Integer,
                val y: Integer,
                val bold: Boolean,
                val height: Integer,
                val pageNo: Integer,
                val locale: Locale,
                val paymentFieldType: PaymentFieldType,
                val properties: Map[CandidateMetadata, Any]) extends Comparable[Candidate] {

  require(Option(value).isDefined)

  override def compareTo(other: Candidate): Int = compare(this, other)

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[Candidate] &&
      other.asInstanceOf[Candidate].value.equals(this.value)
  }

}
