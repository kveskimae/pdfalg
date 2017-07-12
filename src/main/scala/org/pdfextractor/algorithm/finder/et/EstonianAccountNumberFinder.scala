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
import org.pdfextractor.algorithm.finder._

@Service
class EstonianAccountNumberFinder() extends AbstractFinder(PATTERN_ESTONIAN_IBAN_START_WITH_REST_OF_LINE_AS_REGEX, PATTERN_ESTONIAN_IBAN_AS_REGEX, false) {

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
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val ibanStartingPartMatcher = PATTERN_ESTONIAN_IBAN_START_AS_REGEX.pattern.matcher(oneLineContainingIBANStart)
      while (ibanStartingPartMatcher.find()) {
        val accountNumberValueBuilder = new StringBuilder
        val remainingPartStartIdx = ibanStartingPartMatcher.end
        val ibanStart = ibanStartingPartMatcher.group()
        accountNumberValueBuilder.append(ibanStart)
        val remainingPartAfterIBANStart = oneLineContainingIBANStart.substring(remainingPartStartIdx)
        val numbersMatcher = PATTERN_INTEGER_NUMBER.findAllIn(remainingPartAfterIBANStart)
        while ( {
          numbersMatcher.hasNext
        }) {
          val nextNumberPart = numbersMatcher.next()
          accountNumberValueBuilder.append(nextNumberPart)
        }
        val builtAccountNr = accountNumberValueBuilder.toString
        val builtIBANMatcher = PATTERN_ESTONIAN_IBAN_AS_REGEX.findFirstIn(builtAccountNr)
        if (builtIBANMatcher.nonEmpty) {
          val accountNr = builtIBANMatcher.get
          if (isValueAllowed(accountNr)) {
            val candidate = buildCandidate(parseResult, None, accountNr)
            addOneElementToListIfNotAlreadyContained(ret, candidate)
          }
        }
      }
    }
    ret
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = new Candidate(value, 1, 1, false, 1, 1, SupportedLocales.ESTONIA, IBAN, Map.empty)

  protected def buildCandidate(parseResult: ParseResult, phrase: Option[Phrase], value: Any, params: Any*): Candidate = buildCandidate(parseResult, phrase.orNull, value, params)

  override def isValueAllowed(raw: Any): Boolean = {
    val value = raw.asInstanceOf[String]

    if (Option(value).isEmpty) return false
    if (value.length != 20) return false

    // Move the four initial characters to the end of the string.
    val swapped = value.substring(4) + value.substring(0, 4)

    // Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35.
    val numericAccountNumber = new StringBuilder
    swapped.foreach(c => numericAccountNumber.append(Character.getNumericValue(c)))

    // Interpret the string as a decimal integer and compute the remainder of that number on division by 97.
    val ibanNumber = new BigInteger(numericAccountNumber.toString)

    ibanNumber.mod(MAGIC_NUMBER).intValue == 1
  }

  def parseValue(raw: String): Any = raw

  def getType = IBAN

}
