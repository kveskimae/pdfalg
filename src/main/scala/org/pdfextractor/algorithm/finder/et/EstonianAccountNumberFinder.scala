package org.pdfextractor.algorithm.finder.et

import java.math.BigInteger
import java.util.Locale

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianRegexPatterns._
import org.pdfextractor.algorithm.parser.ParseResult
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.IBAN
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@Service
class EstonianAccountNumberFinder extends AbstractFinder(EstIBANStartWithRestOfLineR, EstIBANCorrectR, false) {

  val MagicNo = new BigInteger("97")

  override def getLocale: Locale = SupportedLocales.ESTONIA

  def getType = IBAN

  override def findCandidates(parseResult: ParseResult): Seq[Candidate] = {
    val ret: ListBuffer[Candidate] =
      collection.mutable.ListBuffer.empty[Candidate]
    val linesContainingIBANStart: mutable.Buffer[String] =
      searchPattern.get.findAllIn(parseResult.text).toBuffer
    val linesContainingIBANWithoutSpaces: ListBuffer[String] =
      ListBuffer.empty[String]
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val foundAccountNrValues =
        getValuePattern.findAllIn(oneLineContainingIBANStart)
      if (!foundAccountNrValues.isEmpty)
        linesContainingIBANWithoutSpaces += oneLineContainingIBANStart
      for (accountNrValue <- foundAccountNrValues) {
        if (isValueAllowed(accountNrValue)) {
          val candidate = buildCandidate1(accountNrValue)
          addOneElementToListIfNotAlreadyContained(ret, candidate)
        }
      }
    }
    linesContainingIBANStart --= linesContainingIBANWithoutSpaces
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val ibanStartingPartMatcher =
        EstIBANStartR.pattern.matcher(oneLineContainingIBANStart)
      while (ibanStartingPartMatcher.find()) {
        val accountNumberValueBuilder = new StringBuilder
        val remainingPartStartIdx = ibanStartingPartMatcher.end
        val ibanStart = ibanStartingPartMatcher.group()
        accountNumberValueBuilder.append(ibanStart)
        val remainingPartAfterIBANStart =
          oneLineContainingIBANStart.substring(remainingPartStartIdx)
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
            val candidate = buildCandidate1(accountNr)
            addOneElementToListIfNotAlreadyContained(ret, candidate)
          }
        }
      }
    }
    ret
  }

  def addOneElementToListIfNotAlreadyContained(
                                                oldList: mutable.Buffer[Candidate],
                                                newValue: Candidate): Unit = {
    if (!oldList.contains(newValue)) oldList += newValue
    else {
      var toBeReplaced: Option[Candidate] = None
      for (oldValue <- oldList) {
        if (oldValue.value.equals(newValue.value)) toBeReplaced = Some(oldValue)
      }
      if (toBeReplaced.isDefined && toBeReplaced.get.compareTo(newValue) > 0) {
        oldList -= toBeReplaced.get
        oldList += newValue
      }
    }
  }

  override def isValueAllowed(raw: Any): Boolean = {
    Option(raw).isDefined &&
      raw.isInstanceOf[String] &&
      raw.asInstanceOf[String].length == 20 &&
      isMagicModulusOne(raw.asInstanceOf[String])
  }

  private def isMagicModulusOne(value: String): Boolean = {
    val swapped: String =
      (value.substring(4) + value.substring(0, 4)). // Move the four initial characters to the end of the string.
        map(Character.getNumericValue(_))
        .map(_.toString)
        . // Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35.
        mkString

    new BigInteger(swapped)
      .mod(MagicNo)
      .intValue == 1 // Interpret the string as integer and compute the remainder of that number on division by 97.
  }

  def parseValue(raw: String): Any = raw

}
