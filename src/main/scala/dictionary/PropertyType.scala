package dictionary

sealed trait PropertyType

case object ESTONIAN_IS_PANK_PRESENT extends PropertyType
case object DOUBLE_NUMBER extends PropertyType
case object EURO_SIGN_FOUND extends PropertyType
case object NORMAL_LINE extends PropertyType
case object PHRASE_TYPE extends PropertyType