package finder.et

import java.math.BigInteger

import org.pdfextractor.algorithm.candidate.Candidate
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.REFERENCE_NUMBER
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}

import scala.collection.mutable.ListBuffer

@Service
class EstonianReferenceNumberFinder extends AbstractFinder(PATTERN_ESTONIAN_REFERENCE_NUMBER_LINE_AS_REGEX, PATTERN_ESTONIAN_REFERENCE_NUMBER_DIGITS_AS_REGEX, true) {

  override def parseValue(raw: String): Any = raw

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*) = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, REFERENCE_NUMBER, Map.empty)

  override def getType = REFERENCE_NUMBER

  override def isValueAllowed(raw: Any): Boolean = {
    try {
      val value = new BigInteger(raw.asInstanceOf[String])
      isCorrectFormat(value) && checkDigitMatches(value, calculateCheckDigit(value))
    } catch {
      case e: Exception => false
    }
  }

  private def calculateCheckDigit(value: BigInteger) = {
    val digits: Seq[Integer] = findDigitsUntilOneBeforeLastInReverseOrder(value)
    val productsSum = calculate731Sum(digits)
    val tensMultiple = findTensMultiple(productsSum)
    tensMultiple - productsSum
  }

  private def checkDigitMatches(value: BigInteger, checkDigit: Int) = {
    val lastDigit = Integer.valueOf("" + value.toString.charAt(value.toString.length - 1))
    lastDigit == checkDigit
  }

  private def findTensMultiple(productsSum: Int) = {
    var ret = productsSum
    while ( {
      ret % 10 != 0
    }) ret += 1
    ret
  }

  private def calculate731Sum(digits: Seq[Integer]) = {
    var sum = 0
    var multiplier = 7
    for (digit <- digits) {
      sum += digit * multiplier
      if (multiplier == 7) multiplier = 3
      else if (multiplier == 3) multiplier = 1
      else if (multiplier == 1) multiplier = 7
    }
    sum
  }

  private def findDigitsUntilOneBeforeLastInReverseOrder(value: BigInteger): Seq[Integer] = {
    val ret: ListBuffer[Integer] = ListBuffer.empty[Integer]
    var i = value.toString.length - 2
    while ( {
      i >= 0
    }) {
      val preConversionDigit = "" + value.toString.charAt(i)
      val digit: Integer = Integer.valueOf(preConversionDigit)
      ret += digit

      {
        i -= 1; i + 1 // TODO
      }
    }
    ret.toList
  }

  private def isCorrectFormat(value: BigInteger): Boolean = {
    value != null &&
    value.signum > 0 && // must be positive
    value.toString.length >= 2 && // must have 2 - 20 digits
    value.toString.length <= 20
  }

}
