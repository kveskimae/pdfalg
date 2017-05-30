package finder

import candidate.Candidate
import dictionary.PaymentFieldType
import org.apache.commons.lang3.builder.{ReflectionToStringBuilder, ToStringStyle}

import scala.collection.mutable

class FinderResult {

  private val candidatesBuilder: mutable.MapBuilder[PaymentFieldType, Set[Candidate], Map[PaymentFieldType, Set[Candidate]]] = new scala.collection.mutable.MapBuilder[PaymentFieldType, Set[Candidate], Map[PaymentFieldType, Set[Candidate]]](Map.empty)

  PaymentFieldType.values.foreach(fieldType => {
    val mapElem: Tuple2[PaymentFieldType, Set[Candidate]] = Tuple2(fieldType, Set.empty)
    candidatesBuilder += mapElem
  })

  val candidatesMap: Map[PaymentFieldType, Set[Candidate]] = candidatesBuilder.result()

  def getCandidates(fieldType: PaymentFieldType): Set[Candidate] = {
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
