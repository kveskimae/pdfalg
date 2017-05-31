package regex

import java.util.regex.Pattern

object CommonRegexPatterns {

  val ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND = "[\\-\\w\\sÕõüÜÄäÖöÜüžŽšŠ&]"

  val ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND = "[\\-\\w\\s&]"

  val ITALIAN_INVOICE_ID_WORDS = "(numero|codice|id|n[or]{0,1}[.]{0,1})"

  val MINIMUM_NUMBER_OF_CHARACTERS = 3

  val PATTERN_MINIMUM_CHARACTERS = Pattern.compile("^.*" + ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND + "{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}.*$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

  val VOID_CHARACTER = "[\\s:.€]"

  val DIGITS_WITH_COMMAS_AND_DOTS = "(\\d{1,}[.,\\d]{0,})"

  val EUR = "(eur|€|eurot|euro|in eur)"

  val OPTIONAL_WHITESPACE = "\\s{0,}"

  val PATTERN_VOID: Pattern = // Phrases only containing void characters are ignored when searching right
  // or bottom neighbor phrase
    Pattern.compile("^" + VOID_CHARACTER + "{0,}$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

  // Numbers

  val PATTERN_INTEGER_NUMBER: Pattern = Pattern.compile("(\\d+)")

  val PATTERN_AT_LEAST_2_INTEGER_NUMBERS: Pattern = Pattern.compile("([\\d]{2,})")

  val PATTERN_AT_LEAST_3_INTEGER_NUMBERS: Pattern = Pattern.compile("([\\d]{3,})")

  val PATTERN_DIGITS_WITH_COMMAS_AND_DOTS: Pattern = Pattern.compile(DIGITS_WITH_COMMAS_AND_DOTS)

  // Total

  val PATTERN_EURO_SIGN: Pattern = Pattern.compile("^(.*)" + EUR + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

}
