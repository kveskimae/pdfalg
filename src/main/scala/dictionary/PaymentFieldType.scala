package dictionary

sealed trait PaymentFieldType

case object IBAN extends PaymentFieldType
case object INVOICE_ID extends PaymentFieldType
case object NAME extends PaymentFieldType
case object REFERENCE_NUMBER extends PaymentFieldType
case object TOTAL extends PaymentFieldType
case object TOTAL_BEFORE_TAXES extends PaymentFieldType
case object ISSUE_DATE extends PaymentFieldType
case object DUE_DATE extends PaymentFieldType
case object VATIN extends PaymentFieldType

object PaymentFieldType {

  implicit def convert(name: String): PaymentFieldType = name match {
    case "IBAN" => IBAN
    case "INVOICE_ID" => INVOICE_ID
    case "NAME" => NAME
    case "REFERENCE_NUMBER" => REFERENCE_NUMBER
    case "TOTAL" => TOTAL
    case "TOTAL_BEFORE_TAXES" => TOTAL_BEFORE_TAXES
    case "ISSUE_DATE" => ISSUE_DATE
    case "DUE_DATE" => DUE_DATE
    case "VATIN" => VATIN
    case _ => throw new IllegalArgumentException("Unknown payment field type name: " + name)
  }

  val values: Seq[PaymentFieldType] = Seq(IBAN, INVOICE_ID, NAME, REFERENCE_NUMBER, TOTAL, TOTAL_BEFORE_TAXES, ISSUE_DATE, DUE_DATE, VATIN)

}