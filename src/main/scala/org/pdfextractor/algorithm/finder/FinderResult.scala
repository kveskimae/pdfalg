package org.pdfextractor.algorithm.finder

import org.pdfextractor.algorithm.candidate.Candidate
import org.apache.commons.lang3.builder.{ReflectionToStringBuilder, ToStringStyle}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection.mutable

class FinderResult {

  type CandidatesMapBuilder = mutable.MapBuilder[PaymentFieldType, mutable.Set[Candidate], Map[PaymentFieldType, mutable.Set[Candidate]]]

  private val candidatesBuilder: CandidatesMapBuilder = new scala.collection.mutable.MapBuilder[PaymentFieldType, mutable.Set[Candidate], Map[PaymentFieldType, mutable.Set[Candidate]]](Map.empty)

  type Field2Candidates = Tuple2[PaymentFieldType, mutable.Set[Candidate]]

  PaymentFieldType.values.foreach(fieldType => {
    candidatesBuilder += Tuple2(fieldType, mutable.Set.empty)
  })

  val candidatesMap: Map[PaymentFieldType, mutable.Set[Candidate]] = candidatesBuilder.result()

  def getCandidates(fieldType: PaymentFieldType): mutable.Set[Candidate] = {
    candidatesMap(fieldType)
  }

  override def toString: String = ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE)

  def getValue[T >: Null](fieldType: PaymentFieldType): Option[T] = {
    if (getCandidates(fieldType).isEmpty) {
      None
    } else {
      Some(getCandidates(fieldType).head.asInstanceOf[T])
    }
  }

  def hasValuesForType(fieldType: PaymentFieldType): Boolean = {
    !getCandidates(fieldType).isEmpty
  }

}
