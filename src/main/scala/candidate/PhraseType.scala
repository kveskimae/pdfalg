package candidate

import dictionary.PaymentFieldType
import org.apache.commons.lang3.builder.ReflectionToStringBuilder
import java.util.regex.Pattern

class PhraseType {

  var id: Integer = null
  var locale: String = null
  var paymentFieldType: PaymentFieldType = null
  var keyPhrase: String = null
  var comparisonPart: Integer = null
  var pattern: Pattern = null

  override def toString: String = ReflectionToStringBuilder.toString(this)

  def setKeyPhrase(keyPhrase: String): Unit = {
    this.keyPhrase = keyPhrase
    this.pattern = Pattern.compile("^(.*)(" + keyPhrase + ")(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
  }

}
