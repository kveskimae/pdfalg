package org.pdfextractor.algorithm

import java.math.BigInteger

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns.{PATTERN_ESTONIAN_ORDINARY_TOTAL_LINE_AS_REGEX, PATTERN_ESTONIAN_PANK_AS_REGEX}
import org.pdfextractor.algorithm.parser.Phrase
import org.pdfextractor.algorithm.regex.{PATTERN_EURO_SIGN_AS_REGEX, PATTERN_VOID_AS_REGEX}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID

package object finder {

  private[finder] val FOCUS_TYPE = INVOICE_ID // if you have a trouble with particular field type, set it here & let it log

  val MagicNo = new BigInteger("97")

  def isVoidPhrase(phrase: Phrase): Boolean = {
    Option(phrase.text).isEmpty || isVoidText(phrase.text)
  }

  def isVoidText(text: String): Boolean = PATTERN_VOID_AS_REGEX.pattern.matcher(text).matches

  def combinePhrases1(phrase: Phrase, otherPhrase: Phrase): Phrase = {
    new Phrase(otherPhrase.x, otherPhrase.y, otherPhrase.pageNumber, otherPhrase.height, phrase.width, phrase.text + " " + otherPhrase.text, otherPhrase.bold)
  }

  def isPankPresent(text: String): Boolean = {
    PATTERN_ESTONIAN_PANK_AS_REGEX.findFirstIn(text).nonEmpty
  }

  def countDotsAndCommas(number: String): Int = {
    val replaced = number.replaceAll(",", ".")
    val dotCount = StringUtils.countMatches(replaced, ".")
    dotCount
  }

  def isEuroPresent(text: String): Boolean = {
    PATTERN_EURO_SIGN_AS_REGEX.findFirstIn(text).nonEmpty
  }

  def isNormalTotalLine(text: String) = {
    PATTERN_ESTONIAN_ORDINARY_TOTAL_LINE_AS_REGEX.findFirstIn(text).nonEmpty
  }

}