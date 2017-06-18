package finder.et

import java.math.BigInteger

import candidate.Candidate
import dictionary.{IBAN, SupportedLocales}
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.RegexUtils

import scala.collection.mutable.ListBuffer

object EstonianAccountNumberFinder {

  val MAGIC_NUMBER = new BigInteger("97")

}

@Service
class EstonianAccountNumberFinder(override val phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, PATTERN_ESTONIAN_IBAN_START_WITH_REST_OF_LINE, PATTERN_ESTONIAN_IBAN, false) {

  override def findCandidates(parseResult: ParseResult): Seq[Candidate]  = {
    val ret: ListBuffer[Candidate] = collection.mutable.ListBuffer.empty[Candidate]
    val linesContainingIBANStart: ListBuffer[String] = RegexUtils.findMatches(parseResult.text, searchPattern)
    val linesContainingIBANWithoutSpaces: ListBuffer[String] = ListBuffer.empty[String]
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val foundAccountNrValues = RegexUtils.findMatches(oneLineContainingIBANStart, valuePattern)
      if (!foundAccountNrValues.isEmpty) linesContainingIBANWithoutSpaces += oneLineContainingIBANStart
      for (accountNrValue <- foundAccountNrValues) {
        if (isValueAllowed(accountNrValue)) {
          val candidate = buildCandidate(parseResult, null, accountNrValue)
          addOneElementToListIfNotAlreadyContained(ret, candidate)
        }
      }
    }
    linesContainingIBANStart --= linesContainingIBANWithoutSpaces
    for (oneLineContainingIBANStart <- linesContainingIBANStart) {
      val ibanStartingPartMatcher = PATTERN_ESTONIAN_IBAN_START.matcher(oneLineContainingIBANStart)
      if (ibanStartingPartMatcher.find) {
        val accountNumberValueBuilder = new StringBuilder
        val remainingPartStartIdx = ibanStartingPartMatcher.end
        val ibanStart = ibanStartingPartMatcher.group
        accountNumberValueBuilder.append(ibanStart)
        val remainingPartAfterIBANStart = oneLineContainingIBANStart.substring(remainingPartStartIdx)
        val numbersMatcher = regex.CommonRegexPatterns.PATTERN_INTEGER_NUMBER.matcher(remainingPartAfterIBANStart)
        while ( {
          numbersMatcher.find
        }) {
          val nextNumberPart = numbersMatcher.group
          accountNumberValueBuilder.append(nextNumberPart)
        }
        val builtAccountNr = accountNumberValueBuilder.toString
        val builtIBANMatcher = PATTERN_ESTONIAN_IBAN.matcher(builtAccountNr)
        if (builtIBANMatcher.find) {
          val accountNr = builtIBANMatcher.group
          if (isValueAllowed(accountNr)) {
            val candidate = buildCandidate(parseResult, null, accountNr)
            addOneElementToListIfNotAlreadyContained(ret, candidate)
          }
        }
      }
    }
    ret
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*) = new Candidate(value, 1, 1, false, 1, 1, SupportedLocales.ESTONIA, IBAN, Map.empty)

  def isValueAllowed(raw: Any): Boolean = {
    val value = raw.asInstanceOf[String]

    if (value == null) return false
    if (value.length != 20) return false

    // Move the four initial characters to the end of the string.
    val swapped = value.substring(4) + value.substring(0, 4)

    // Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35.
    val numericAccountNumber = new StringBuilder
    swapped.foreach(c => numericAccountNumber.append(Character.getNumericValue(c)))

    // Interpret the string as a decimal integer and compute the remainder of that number on division by 97.
    val ibanNumber = new BigInteger(numericAccountNumber.toString)

    ibanNumber.mod(EstonianAccountNumberFinder.MAGIC_NUMBER).intValue == 1
  }

  def parseValue(raw: String): Any = raw

  def getType = IBAN

}
