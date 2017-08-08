package org.pdfextractor.algorithm

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns.{EstPankR, EstTotalR}
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.regex.{EurR, IgnoredR}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.INVOICE_ID

package object finder {

  val FOCUS_TYPE = INVOICE_ID // if you have a trouble with particular field type, set it here & let it log

  def isVoidPhrase(phrase: Phrase): Boolean = {
    Option(phrase.text).isEmpty || isVoidText(phrase.text)
  }

  def isVoidText(text: String): Boolean = IgnoredR.pattern.matcher(text).matches

  def combinePhrases1(phrase: Phrase, otherPhrase: Phrase): Phrase = {
    new Phrase(otherPhrase.x,
      otherPhrase.y,
      otherPhrase.pageNumber,
      otherPhrase.height,
      phrase.width,
      phrase.text + " " + otherPhrase.text,
      otherPhrase.bold)
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

  def findTensMultiple(acc: Int): Int = {
    if (acc % 10 == 0) acc else findTensMultiple(acc + 1)
  }

  def calculate731Sum(digits: Seq[Int]) = {
    def shiftMultliplier(previous: Int): Int = {
      if (previous == 7) 3
      else if (previous == 3) 1
      else if (previous == 1) 7
      else throw new IllegalArgumentException("Illegal multiplier: " + previous)
    }

    var nextMultiplier = 7

    digits.fold(0)(
      (acc: Int, digit: Int) => {
        val multiplier = nextMultiplier
        nextMultiplier = shiftMultliplier(nextMultiplier)
        acc + digit * multiplier
      }
    )
  }

  def digitsToPenultimateInReverse(value: BigInt): Seq[Int] = {
    (0 to (value.toString.length - 2)).
      reverse.
      map(value.toString.charAt(_)).
      map(_.asDigit)
  }

  def searchRight(cur: Option[Phrase], parseResult: ParseResult): Option[Phrase] = {
    if (cur.isEmpty) {
      None
    } else if (isVoidPhrase(cur.get)) {
      searchRight(parseResult.findClosestPhraseOnRight(cur.get), parseResult)
    } else {
      cur
    }
  }

}
