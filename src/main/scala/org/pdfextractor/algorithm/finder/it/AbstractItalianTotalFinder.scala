package org.pdfextractor.algorithm.finder.it

import java.text.{DecimalFormat, DecimalFormatSymbols, ParseException}
import java.util.Locale

import org.pdfextractor.algorithm.candidate._
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.pdfextractor.algorithm.finder.et.EstonianTotalFinder
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

@Service
abstract class AbstractItalianTotalFinder extends AbstractFinder(None, None, true, true) {
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
	
	private[it] val PATTERN_ITALIAN_ORDINARY_TOTAL_LINE_AS_REGEX = ("""^(?ims).{0,30}:([\s]{0,})""" + EUR + """?([\s]{0,})""" + DIGITS_WITH_COMMAS_AND_DOTS + """([\s]{0,})""" + EUR + """?([\s]{0,})$""").r

	@org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
	def refreshed(): Unit = {
		searchPattern = Some(("^(?ims)(.*)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, getType) + "(.*)$").r)
		valuePattern = Some(PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX)
	}

	override protected def searchValuesFromPhrase(phrase: Phrase, parseResult: ParseResult, valuePattern2: Regex): ListBuffer[Candidate] = {
		val ret: ListBuffer[Candidate] = ListBuffer.empty
		val doubleValues = searchForDoubleValues(phrase.text)
		if (doubleValues.size == 1) {
			val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findAllIn(phrase.text)
			while ( {
				totalAsNumberMatcher.hasNext
			}) {
				val totalAsString = totalAsNumberMatcher.next()
				val candidate: Option[Candidate] = findCandidateValue(totalAsString, phrase, parseResult)
				if (candidate.isDefined) ret += candidate.get
			}
		}
		else {
			val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findAllIn(phrase.text)
			var biggest: Option[Candidate] = None
			while ( {
				totalAsNumberMatcher.hasNext
			}) {
				val totalAsString = totalAsNumberMatcher.next()
				val candidate: Option[Candidate] = findCandidateValue(totalAsString, phrase, parseResult)
				if (biggest.isEmpty) biggest = candidate
				else if (Option(candidate).isDefined && candidate.get.value.asInstanceOf[Double] > biggest.get.value.asInstanceOf[Double]) biggest = candidate
			}
			if (biggest.isDefined) ret += biggest.get
		}
		ret
	}

	private def findCandidateValue(totalAsString: String, phrase: Phrase, parseResult: ParseResult): Option[Candidate] = {
		val dotCount = EstonianTotalFinder.countDotsAndCommas(totalAsString)
		val `type` = phraseTypesStore.findType(SupportedLocales.ITALY, getType, phrase.text)
		if (dotCount < 2) {
			val replaced = totalAsString.replaceAll(",", ".")
			val totalAsDouble: Double = replaced.toDouble
			val doubleNumber = dotCount > 0
			val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
			Some(candidate)
		}
		else try {
      var totalAsDouble = .0
      if (isDotThousandsSeparator(totalAsString)) totalAsDouble = decimalFormatWithDotAsThousandsSeparator.parse(totalAsString).doubleValue
      else totalAsDouble = decimalFormatWithCommaAsThousandsSeparator.parse(totalAsString).doubleValue
      val doubleNumber = isDouble(totalAsDouble)
      val candidate = buildCandidate(parseResult, phrase, totalAsDouble, doubleNumber, `type`)
      Some(candidate)
    } catch {
      case ignored: ParseException =>
        None
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
		PATTERN_EURO_SIGN_AS_REGEX.findFirstIn(text).nonEmpty
	}

	private def isNormalTotalLine(text: String): Boolean = {
		PATTERN_ITALIAN_ORDINARY_TOTAL_LINE_AS_REGEX.findFirstIn(text).nonEmpty
	}

	override def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
		val doubleNumber = params(0).asInstanceOf[Boolean]
		val `type` = params(1).asInstanceOf[PhraseType]
		val euroSignFound = isEuroPresent(phrase.text)
		val normalTotalLine = isNormalTotalLine(phrase.text)
		val properties: Map[PropertyType, Any] = Map(DOUBLE_NUMBER -> doubleNumber, PHRASE_TYPE -> `type`, EURO_SIGN_FOUND -> euroSignFound, NORMAL_LINE -> normalTotalLine)
		val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, getType, properties)
		ret
	}

	override def isValueAllowed(value: Any) = true

	def parseValue(raw: String) = throw new UnsupportedOperationException

}
