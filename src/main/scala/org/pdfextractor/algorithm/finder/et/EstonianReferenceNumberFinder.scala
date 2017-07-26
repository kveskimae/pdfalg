package org.pdfextractor.algorithm.finder.et

import java.math.BigInteger

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.REFERENCE_NUMBER
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}

import scala.collection.mutable.ListBuffer

@Service
class EstonianReferenceNumberFinder
    extends AbstractFinder(EstRefNoLineR, EstRefNoR) {

  override def parseValue(raw: String): Any = raw

  override protected def buildCandidate(parseResult: ParseResult,
                                        phrase: Phrase,
                                        value: Any,
                                        params: Any*) =
    new Candidate(value,
                  phrase.x,
                  phrase.y,
                  phrase.bold,
                  phrase.height,
                  phrase.pageNumber,
                  SupportedLocales.ESTONIA,
                  REFERENCE_NUMBER,
                  Map.empty)

  override def getType = REFERENCE_NUMBER

  override def isValueAllowed(raw: Any): Boolean = {
    try {
      val value = new BigInteger(raw.asInstanceOf[String])
      isCorrectFormat(value) && checkDigitMatches(value,
                                                  calculateCheckDigit(value))
    } catch {
      case e: Exception => false
    }
  }

  private def calculateCheckDigit(value: BigInteger) = {
    val productsSum = calculate731Sum(digitsToPenultimateInReverse(value))

    findTensMultiple(productsSum) - productsSum
  }

  private def checkDigitMatches(value: BigInteger, checkDigit: Int) = {
    val lastDigit =
      Integer.valueOf("" + value.toString.charAt(value.toString.length - 1))
    lastDigit == checkDigit
  }

  private def isCorrectFormat(value: BigInteger): Boolean = {
    Option(value).isDefined &&
    value.signum > 0 && // must be positive
    value.toString.length >= 2 && // must have 2 - 20 digits
    value.toString.length <= 20
  }

}
