package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.regex._

// Ä, ä \u00c4, \u00e4
// Ö, ö \u00d6, \u00f6
// Ü, ü \u00dc, \u00fc
object EstonianRegexPatterns {

  val EstIBANStart = Est2IBANStartLetters + """(\d{2})"""
  val EstIBANStartR = (EstIBANStart).r
  // IBAN is 18 digits https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country
  val EstIBANCorrectR = (raw"""$Est2IBANStartLetters(\d{18})""").r
  val EstIBANStartWithRestOfLineR = (raw"""^(?ism)(.*)$EstIBANStart(.*)$$""").r
  // Total
  val EstTotalR =
    (raw"""^(?ism).{0,30}:([\s]{0,})$Eur?([\s]{0,})$DigitsAndCommas([\s]{0,})$Eur?([\s]{0,})$$""").r
  val EstPankR = ("^(?ism)(.*)(pank)(.*)$").r
  // Reference number
  val EstRefNoLineR = ("(?ism)(viite)(.*)$").r
  // 2-20 digits http://www.pangaliit.ee/et/arveldused/viitenumber
  val EstRefNoR = ("""(?m)(\d{2,20})""").r
  private val Est2IBANStartLetters = """(EE)"""

}
