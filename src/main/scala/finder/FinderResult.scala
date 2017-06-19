package finder

import candidate.Candidate
import org.apache.commons.lang3.builder.{ReflectionToStringBuilder, ToStringStyle}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection.mutable

class FinderResult {

  private val candidatesBuilder: mutable.MapBuilder[PaymentFieldType, mutable.Set[Candidate], Map[PaymentFieldType, mutable.Set[Candidate]]] = new scala.collection.mutable.MapBuilder[PaymentFieldType, mutable.Set[Candidate], Map[PaymentFieldType, mutable.Set[Candidate]]](Map.empty)

  PaymentFieldType.values.foreach(fieldType => {
    val mapElem: Tuple2[PaymentFieldType, mutable.Set[Candidate]] = Tuple2(fieldType, mutable.Set.empty)
    candidatesBuilder += mapElem
  })

  val candidatesMap: Map[PaymentFieldType, mutable.Set[Candidate]] = candidatesBuilder.result()

  def getCandidates(fieldType: PaymentFieldType): mutable.Set[Candidate] = {
    candidatesMap(fieldType)
  }

  override def toString: String = ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE)

  def getValue[T >: Null](fieldType: PaymentFieldType): T = {
    if (getCandidates(fieldType).isEmpty) {
      null
    } else {
      getCandidates(fieldType).head.asInstanceOf[T]
    }
  }

  def hasValuesForType(fieldType: PaymentFieldType): Boolean = {
    !getCandidates(fieldType).isEmpty
  }

}
