package finder.et

import org.pdfextractor.algorithm.regex._

// Ä, ä \u00c4, \u00e4
// Ö, ö \u00d6, \u00f6
// Ü, ü \u00dc, \u00fc
object EstonianRegexPatterns {

  val ESTONIAN_IBAN_STARTING_LETTERS = """(EE)"""

  val ESTONIAN_IBAN_START: String = ESTONIAN_IBAN_STARTING_LETTERS + """(\d{2})"""

  // IBAN is 18 digits https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country
  val PATTERN_ESTONIAN_IBAN_AS_REGEX = (raw"""$ESTONIAN_IBAN_STARTING_LETTERS(\d{18})""").r

  val PATTERN_ESTONIAN_IBAN_START_AS_REGEX = (ESTONIAN_IBAN_START).r

  val PATTERN_ESTONIAN_IBAN_START_WITH_REST_OF_LINE_AS_REGEX = (raw"""^(?ism)(.*)$ESTONIAN_IBAN_START(.*)$$""").r

  // Total
  val PATTERN_ESTONIAN_ORDINARY_TOTAL_LINE_AS_REGEX = (raw"""^(?ism).{0,30}:([\s]{0,})$EUR?([\s]{0,})$DIGITS_WITH_COMMAS_AND_DOTS([\s]{0,})$EUR?([\s]{0,})$$""").r

  val PATTERN_ESTONIAN_PANK_AS_REGEX = ("^(?ism)(.*)(pank)(.*)$").r

  // Reference number
  val PATTERN_ESTONIAN_REFERENCE_NUMBER_LINE_AS_REGEX = ("(?ism)(viite)(.*)$").r

  // 2-20 digits http://www.pangaliit.ee/et/arveldused/viitenumber
  val PATTERN_ESTONIAN_REFERENCE_NUMBER_DIGITS_AS_REGEX = ("""(?m)(\d{2,20})""").r

}
