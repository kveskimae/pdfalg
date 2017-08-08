package org.pdfextractor.algorithm

import org.apache.commons.lang3.StringUtils

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

package object regex {

  val EstNameLetter = """[\-\w\sÕõüÜÄäÖöÜüžŽšŠ&]"""

  val VoidLetter = """[\s:.€]"""

  val OptionalWhitespace = """\s{0,}"""

  val ItInvoiceIDWord = """(numero|codice|id|n[or]{0,1}[.]{0,1})"""

  val ItNameLetter = """[\-\w\s&]"""

  val ItNameMinNoLetters = 3

  val ItNameMinR = (raw"^(?ism).*$ItNameLetter{$ItNameMinNoLetters,}.*$$").r

  // Phrases only containing void characters are ignored when searching right
  // or bottom neighbor phrase
  val IgnoredR: Regex = (raw"""^(?ism)$VoidLetter{0,}$$""").r

  // Numbers

  val DigitsR = """(\d+)""".r

  val DigitsAndCommas = """(\d{1,}[.,\d]{0,})"""

  val DigitsAndCommasR = DigitsAndCommas.r

  val TwoOrMoreDigitsR = """([\d]{2,})""".r

  // Total

  val Eur = "(eur|€|eurot|euro|in eur)"

  val EurR = (raw"^(?ism)(.*)$Eur(.*)$$").r

  def searchForEstonianDoubleValuesAfterText(
                                              searchString: String): Seq[Double] = {
    searchForDoubleValues(searchString)
      .map(_.replaceAll(",", "."))
      .filter(StringUtils.countMatches(_, ".") < 2)
      .map(_.toDouble)
  }

  def searchForDoubleValues(searchString: String): Seq[String] = {
    val excludeDates: (Match) => Boolean = (matched: Match) => {
      val endIdx = matched.end
      val startIdx = matched.start - 1

      (endIdx >= searchString.length || (searchString.charAt(endIdx) != '/' && searchString
        .charAt(endIdx) != '-')) &&
        (startIdx < 0 || searchString.charAt(startIdx) != '/')
    }

    DigitsAndCommasR
      .findAllIn(searchString)
      .matchData
      .filter(excludeDates)
      .map(_.matched)
      .toSeq
  }

}
