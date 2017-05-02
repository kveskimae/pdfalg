package dictionary

sealed trait PaymentFieldType

case object IBAN extends PaymentFieldType
case object INVOICE_ID extends PaymentFieldType
case object NAME extends PaymentFieldType
case object REFERENCE_NUMBER extends PaymentFieldType
case object TOTAL_BEFORE_TAXES extends PaymentFieldType
case object ISSUE_DATE extends PaymentFieldType
case object DUE_DATE extends PaymentFieldType
case object VATIN extends PaymentFieldType