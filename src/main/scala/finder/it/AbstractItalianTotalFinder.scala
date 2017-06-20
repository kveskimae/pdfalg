package finder.it

import java.text.{DecimalFormat, DecimalFormatSymbols, ParseException}
import java.util.Locale
import java.util.regex.Pattern

import candidate.Candidate
import dictionary._
import finder.AbstractFinder
import finder.et.EstonianTotalFinder
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore
import regex.CommonRegexPatterns._
import regex.RegexUtils

import scala.collection.mutable.ListBuffer

@Service
abstract class AbstractItalianTotalFinder(phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, null, null, true, true) {
	// TODO What about thread safety?

	val otherSymbolsForCommaAsThousandsSeparator : DecimalFormatSymbols= new DecimalFormatSymbols(Locale.ITALY)
	otherSymbolsForCommaAsThousandsSeparator.setDecimalSeparator('.')
	otherSymbolsForCommaAsThousandsSeparator.setGroupingSeparator(',')
	
	val decimalFormatWithCommaAsThousandsSeparator: DecimalFormat  = new DecimalFormat("###,###.##", otherSymbolsForCommaAsThousandsSeparator)
	
	val otherSymbolsForDotAsThousandsSeparator: DecimalFormatSymbols= new DecimalFormatSymbols(Locale.ITALY)
	otherSymbolsForDotAsThousandsSeparator.setDecimalSeparator(',')
	otherSymbolsForDotAsThousandsSeparator.setGroupingSeparator('.')
	
	// Even if dot is used as radix character (decimal separator), pattern is always defined with comma as separator
	val decimalFormatWithDotAsThousandsSeparator: DecimalFormat = new DecimalFormat("###,###.##", otherSymbolsForDotAsThousandsSeparator)
	
	private[it] val PATTERN_ITALIAN_ORDINARY_TOTAL_LINE = Pattern.compile("^.{0,30}:([\\s]{0,})" + EUR + "?([\\s]{0,})" + DIGITS_WITH_COMMAS_AND_DOTS + "([\\s]{0,})" + EUR + "?([\\s]{0,})$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)

	def refreshed(): Unit = {
		searchPattern = Pattern.compile("^(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, getType) + "(.*)$", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
		valuePattern = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS
	}

	override protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Pattern): ListBuffer[Candidate] = {
		val ret: ListBuffer[Candidate] = ListBuffer.empty
		val doubleValues = RegexUtils.searchForDoubleValues(phrase.text)
		if (doubleValues.size == 1) {
			val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS.matcher(phrase.text)
			while ( {
				totalAsNumberMatcher.find
			}) {
				val totalAsString = totalAsNumberMatcher.group
				val candidate: Candidate = findCandidateValue(totalAsString, phrase, parseResult)
				if (candidate != null) ret += candidate
			}
		}
		else {
			val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS.matcher(phrase.text)
			var biggest: Candidate = null
			while ( {
				totalAsNumberMatcher.find
			}) {
				val totalAsString = totalAsNumberMatcher.group
				val candidate = findCandidateValue(totalAsString, phrase, parseResult)
				if (biggest == null) biggest = candidate
				else if (candidate != null && candidate.value.asInstanceOf[Double] > biggest.value.asInstanceOf[Double]) biggest = candidate
			}
			if (biggest != null) ret += biggest
		}
		ret
	}

	private def findCandidateValue(totalAsString: String, phrase: Phrase, parseResult: ParseResult): Candidate = {
		val dotCount = EstonianTotalFinder.countDotsAndCommas(totalAsString)
		val `type` = phraseTypesStore.findType(SupportedLocales.ITALY, getType, phrase.text)
		if (dotCount < 2) {
			val replaced = totalAsString.replaceAll(",", ".")
			val totalAsDouble: Double = replaced.toDouble
			val doubleNumber = dotCount > 0
			val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
			candidate
		}
		else try {
      var totalAsDouble = .0
      if (isDotThousandsSeparator(totalAsString)) totalAsDouble = decimalFormatWithDotAsThousandsSeparator.parse(totalAsString).doubleValue
      else totalAsDouble = decimalFormatWithCommaAsThousandsSeparator.parse(totalAsString).doubleValue
      val doubleNumber = isDouble(totalAsDouble)
      val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
      candidate
    } catch {
      case ignored: ParseException =>
        null
    }
	}

	def isDotThousandsSeparator(totalAsString: String): Boolean = {
		if (totalAsString.contains(",") && totalAsString.contains(".")) {
			val firstCommaIdx = totalAsString.indexOf(',')
			val firstDotIdx = totalAsString.indexOf('.')
			return firstDotIdx < firstCommaIdx
		}
		false
	}

	def isDouble(number: Double): Boolean = (number % 1) != 0

	def isEuroPresent(text: String): Boolean = {
		val ret = RegexUtils.patternExistsInText(text, PATTERN_EURO_SIGN)
		ret
	}

	private def isNormalTotalLine(text: String): Boolean = {
		val ret = RegexUtils.patternExistsInText(text, PATTERN_ITALIAN_ORDINARY_TOTAL_LINE)
		ret
	}

	override def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
		val doubleNumber = params(0).asInstanceOf[Boolean]
		val `type` = params(1).asInstanceOf[Nothing]
		val euroSignFound = isEuroPresent(phrase.text)
		val normalTotalLine = isNormalTotalLine(phrase.text)
		val properties: Map[PropertyType, Any] = Map(DOUBLE_NUMBER -> doubleNumber, PHRASE_TYPE -> `type`, EURO_SIGN_FOUND -> euroSignFound, NORMAL_LINE -> normalTotalLine)
		val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, getType, properties)
		ret
	}

	override def isValueAllowed(value: Any) = true

	def parseValue(raw: String) = throw new UnsupportedOperationException

}