package regex

import scala.util.matching.Regex

object CommonRegex {

  val ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND = "[\\-\\w\\sÕõüÜÄäÖöÜüžŽšŠ&]"

  val ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND = "[\\-\\w\\s&]"

  val ITALIAN_INVOICE_ID_WORDS = "(numero|codice|id|n[or]{0,1}[.]{0,1})"

  val MINIMUM_NUMBER_OF_CHARACTERS = 3

  val PATTERN_MINIMUM_CHARACTERS_AS_REGEX = ("^(?ism).*" + ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND + "{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}.*$").r

  val VOID_CHARACTER = "[\\s:.€]"

  val DIGITS_WITH_COMMAS_AND_DOTS = "(\\d{1,}[.,\\d]{0,})"

  val EUR = "(eur|€|eurot|euro|in eur)"

  val OPTIONAL_WHITESPACE = "\\s{0,}"

  // Phrases only containing void characters are ignored when searching right
  // or bottom neighbor phrase
  val PATTERN_VOID_AS_REGEX: Regex = ("^(?ism)" + VOID_CHARACTER + "{0,}$").r

  // Numbers

  val PATTERN_INTEGER_NUMBER = "(\\d+)".r

  val PATTERN_INTEGER_NUMBER_AS_REGEX: Regex = "(\\d+)".r

  val PATTERN_AT_LEAST_2_INTEGER_NUMBERS_AS_REGEX = "([\\d]{2,})".r

  val PATTERN_AT_LEAST_3_INTEGER_NUMBERS_AS_REGEX = "([\\d]{3,})".r

  val PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX = DIGITS_WITH_COMMAS_AND_DOTS.r

  // Total

  val PATTERN_EURO_SIGN_AS_REGEX = ("^(?ism)(.*)" + EUR + "(.*)$").r

}
