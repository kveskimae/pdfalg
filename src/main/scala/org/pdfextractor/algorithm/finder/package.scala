package org.pdfextractor.algorithm

import java.math.BigInteger

import org.pdfextractor.algorithm.parser.Phrase
import org.pdfextractor.algorithm.regex.PATTERN_VOID_AS_REGEX
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID

package object finder {

  private[finder] val FOCUS_TYPE = INVOICE_ID // if you have a trouble with particular field type, set it here & let it log

  def isVoidPhrase(phrase: Phrase): Boolean = {
    if (Option(phrase.text).isDefined) return isVoidText(phrase.text)
    true
  }

  def isVoidText(text: String): Boolean = PATTERN_VOID_AS_REGEX.pattern.matcher(text).matches

  def combinePhrases1(phrase: Phrase, otherPhrase: Phrase): Phrase = {
    val ret = new Phrase(otherPhrase.x, otherPhrase.y, otherPhrase.pageNumber, otherPhrase.height, phrase.width, phrase.text + " " + otherPhrase.text, otherPhrase.bold)
    ret
  }
  val MAGIC_NUMBER = new BigInteger("97")

}