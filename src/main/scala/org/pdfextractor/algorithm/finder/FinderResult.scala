package org.pdfextractor.algorithm.finder

import org.apache.commons.lang3.builder.{ReflectionToStringBuilder, ToStringStyle}
import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection.mutable

class FinderResult {

  val candidatesMap: Map[PaymentFieldType, mutable.Set[Candidate]] =
    PaymentFieldType.values
      .zip(
        List.fill(PaymentFieldType.values.size)(mutable.Set.empty[Candidate])
      )
      .toMap

  override def toString: String =
    ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE)

  def getValue[T >: Null](fieldType: PaymentFieldType): Option[T] = {
    if (getCandidates(fieldType).nonEmpty) {
      Some(getCandidates(fieldType).head.asInstanceOf[T])
    } else {
      None
    }
  }

  def getCandidates(fieldType: PaymentFieldType): mutable.Set[Candidate] = {
    candidatesMap(fieldType)
  }

  def hasValuesForType(fieldType: PaymentFieldType): Boolean =
    getCandidates(fieldType).nonEmpty

}
