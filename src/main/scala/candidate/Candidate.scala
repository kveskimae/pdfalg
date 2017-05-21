package candidate

import java.util.Locale

import dictionary.{PaymentFieldType, PropertyType}
import org.apache.commons.lang3.builder.{HashCodeBuilder, ReflectionToStringBuilder, ToStringStyle}

/*
* Note: Natural orderings in subclass implementations of Comparable are not consistent with equals.
*
* Also rearranges more likely candidates to the start of the collection.
*/
class Candidate(val value: Any,
                val x: Integer,
                val y: Integer,
                val bold: Boolean,
                val height: Integer,
                val pageNo: Integer,
                val locale: Locale,
                val paymentFieldType: PaymentFieldType,
                val properties: Map[PropertyType, Any]) extends Comparable[Candidate] {

  def compareTo(other: Candidate): Int = CandidateComparator.compare(this, other)

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[Candidate]) {
      val otherAsAbstractCandidate = other.asInstanceOf[Candidate]
      if (otherAsAbstractCandidate.value.equals(this.value)) return true
    }
    false
  }

  override def hashCode: Int = HashCodeBuilder.reflectionHashCode(this.value)

  override def toString: String = ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE)

}
