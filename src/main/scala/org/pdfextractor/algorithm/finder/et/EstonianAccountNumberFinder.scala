package org.pdfextractor.algorithm.finder.et

import java.math.BigInteger

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns._
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.IBAN
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@Service
class EstonianAccountNumberFinder extends AbstractFinder(EstIBANStartWithRestOfLineR, EstIBANCorrectR, false) {

  val MagicNo = new BigInteger("97")

  override def findCandidates(parseResult: ParseResult): Seq[Candidate]  = {
    val ret: ListBuffer[Candidate] = collection.mutable.ListBuffer.empty[Candidate]
    val linesContainingIBANStart: mutable.Buffer[String] = getSearchPattern.findAllIn(parseResult.text).toBuffer
    val linesContainingIBANWithoutSpaces: ListBuffer[String] = ListBuffer.empty[String]
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val foundAccountNrValues = getValuePattern.findAllIn(oneLineContainingIBANStart)
      if (!foundAccountNrValues.isEmpty) linesContainingIBANWithoutSpaces += oneLineContainingIBANStart
      for (accountNrValue <- foundAccountNrValues) {
        if (isValueAllowed(accountNrValue)) {
          val candidate = buildCandidate(parseResult, None, accountNrValue)
          addOneElementToListIfNotAlreadyContained(ret, candidate)
        }
      }
    }
    linesContainingIBANStart --= linesContainingIBANWithoutSpaces
    println("1 ret = " + ret.toSeq.toString())
    println("linesContainingIBANStart=" + linesContainingIBANStart)
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val ibanStartingPartMatcher = EstIBANStartR.pattern.matcher(oneLineContainingIBANStart)
      while (ibanStartingPartMatcher.find()) {
        val accountNumberValueBuilder = new StringBuilder
        val remainingPartStartIdx = ibanStartingPartMatcher.end
        val ibanStart = ibanStartingPartMatcher.group()
        accountNumberValueBuilder.append(ibanStart)
        val remainingPartAfterIBANStart = oneLineContainingIBANStart.substring(remainingPartStartIdx)
        val numbersMatcher = DigitsR.findAllIn(remainingPartAfterIBANStart)
        while ( {
          numbersMatcher.hasNext
        }) {
          val nextNumberPart = numbersMatcher.next()
          accountNumberValueBuilder.append(nextNumberPart)
        }
        val builtAccountNr = accountNumberValueBuilder.toString
        val builtIBANMatcher = EstIBANCorrectR.findFirstIn(builtAccountNr)
        if (builtIBANMatcher.nonEmpty) {
          val accountNr = builtIBANMatcher.get
          if (isValueAllowed(accountNr)) {
            val candidate = buildCandidate(parseResult, None, accountNr)
            addOneElementToListIfNotAlreadyContained(ret, candidate)
          }
        }
      }
    }
    println("2 ret = " + ret.toSeq.toString())
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = new Candidate(value, 1, 1, false, 1, 1, SupportedLocales.ESTONIA, IBAN, Map.empty)

  protected def buildCandidate(parseResult: ParseResult, phrase: Option[Phrase], value: Any, params: Any*): Candidate = buildCandidate(parseResult, phrase.orNull, value, params)

  private def isMagicModulusOne(value: String): Boolean = {
    val swapped: String =
      (value.substring(4) + value.substring(0, 4)). // Move the four initial characters to the end of the string.
      map(Character.getNumericValue(_)).map(_.toString). // Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35.
      mkString

    new BigInteger(swapped).mod(MagicNo).intValue == 1 // Interpret the string as integer and compute the remainder of that number on division by 97.
  }

  override def isValueAllowed(raw: Any): Boolean = {
    Option(raw).isDefined &&
    raw.isInstanceOf[String] &&
    raw.asInstanceOf[String].length == 20 &&
    isMagicModulusOne(raw.asInstanceOf[String])
  }

  def parseValue(raw: String): Any = raw

  def getType = IBAN

}
