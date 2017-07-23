package org.pdfextractor.algorithm

import java.util

import org.apache.commons.lang3.StringUtils

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

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

  def searchForEstonianDoubleValuesAfterText(searchString: String): util.List[Double] = {
    val ret = new util.ArrayList[Double]
    val foundDoubles: Iterator[String] = searchForDoubleValues(searchString).iterator
    for (totalAsString: String <- foundDoubles) {
      val totalAsStringReplaced = totalAsString.replaceAll(",", ".")
      val dotCount = StringUtils.countMatches(totalAsStringReplaced, ".")
      if (dotCount < 2) {
        val totalAsDouble: Double = totalAsStringReplaced.toDouble
        ret.add(totalAsDouble)
      }
    }
    ret
  }

  def searchForDoubleValues(searchString: String): List[String] = {
    var ret: ListBuffer[String] = scala.collection.mutable.ListBuffer.empty[String]
    val totalAsNumberMatcher = DigitsAndCommasR.findAllIn(searchString)
    while ( {
      totalAsNumberMatcher.hasNext
    }) {
      val totalAsString = totalAsNumberMatcher.next
      val endIdx = totalAsNumberMatcher.end
      val startIdx = totalAsNumberMatcher.start - 1
      var include = true
      if (endIdx < searchString.length) {
        val charAtEnd = searchString.charAt(endIdx)
        if (charAtEnd == '/' || charAtEnd == '-') include = false
      }
      if (startIdx >= 0) {
        val charAtStart = searchString.charAt(startIdx)
        if (charAtStart == '/') include = false
      }
      if (include) ret += totalAsString
    }
    ret.toList
  }

}