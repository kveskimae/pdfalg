package org.pdfextractor.algorithm

import java.math.BigInteger

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns.{EstTotalR, EstPankR}
import org.pdfextractor.algorithm.parser.Phrase
import org.pdfextractor.algorithm.regex.{EurR, IgnoredR}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID

package object finder {

  private[finder] val FOCUS_TYPE = INVOICE_ID // if you have a trouble with particular field type, set it here & let it log

  def isVoidPhrase(phrase: Phrase): Boolean = {
    Option(phrase.text).isEmpty || isVoidText(phrase.text)
  }

  def isVoidText(text: String): Boolean = IgnoredR.pattern.matcher(text).matches

  def combinePhrases1(phrase: Phrase, otherPhrase: Phrase): Phrase = {
    new Phrase(otherPhrase.x, otherPhrase.y, otherPhrase.pageNumber, otherPhrase.height, phrase.width, phrase.text + " " + otherPhrase.text, otherPhrase.bold)
  }

  def isPankPresent(text: String): Boolean = {
    EstPankR.findFirstIn(text).nonEmpty
  }

  def countDotsAndCommas(number: String): Int = {
    val replaced = number.replaceAll(",", ".")
    val dotCount = StringUtils.countMatches(replaced, ".")
    dotCount
  }

  def isEuroPresent(text: String): Boolean = {
    EurR.findFirstIn(text).nonEmpty
  }

  def isNormalTotalLine(text: String) = {
    EstTotalR.findFirstIn(text).nonEmpty
  }

}