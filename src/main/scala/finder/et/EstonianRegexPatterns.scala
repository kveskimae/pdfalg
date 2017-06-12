package finder.et

import java.util.regex.Pattern
import regex.CommonRegexPatterns._

// Ä, ä \u00c4, \u00e4
// Ö, ö \u00d6, \u00f6
// Ü, ü \u00dc, \u00fc
object EstonianRegexPatterns {

  val ESTONIAN_IBAN_STARTING_LETTERS = "(EE)"

  val ESTONIAN_IBAN_START: String = ESTONIAN_IBAN_STARTING_LETTERS + "(\\d{2})"

  // IBAN is 18 digits https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country
  val PATTERN_ESTONIAN_IBAN: Pattern = Pattern.compile(ESTONIAN_IBAN_STARTING_LETTERS + "(\\d{18})")

  val PATTERN_ESTONIAN_IBAN_START: Pattern = Pattern.compile(ESTONIAN_IBAN_START)

  val PATTERN_ESTONIAN_IBAN_START_WITH_REST_OF_LINE: Pattern = Pattern.compile("^(.*)" + ESTONIAN_IBAN_START + "(.*)$", Pattern.MULTILINE)

  // Total
  val PATTERN_ESTONIAN_ORDINARY_TOTAL_LINE: Pattern = Pattern.compile("^.{0,30}:([\\s]{0,})" + EUR + "?([\\s]{0,})" + DIGITS_WITH_COMMAS_AND_DOTS + "([\\s]{0,})" + EUR + "?([\\s]{0,})$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

  val PATTERN_ESTONIAN_PANK: Pattern = Pattern.compile("^(.*)(pank)(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

  // Reference number
  val PATTERN_ESTONIAN_REFERENCE_NUMBER_LINE: Pattern = Pattern.compile("(viite)(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

  // 2-20 digits http://www.pangaliit.ee/et/arveldused/viitenumber
  val PATTERN_ESTONIAN_REFERENCE_NUMBER_DIGITS: Pattern = Pattern.compile("(\\d{2,20})", Pattern.MULTILINE)

}
