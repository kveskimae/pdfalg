package org.pdfextractor.algorithm.finder.it

import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

import org.pdfextractor.algorithm.candidate.{Candidate, PHRASE_TYPE, PropertyType}
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.ISSUE_DATE
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}

@Service
class ItalianIssueDateFinder extends AbstractFinder(PATTERN_ITALIAN_DATE_AS_REGEX, PATTERN_ITALIAN_DATE_AS_REGEX, false) {

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type`: PhraseType = findType(parseResult, phrase)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, ISSUE_DATE, properties)
    ret
  }

  private def findType(parseResult: ParseResult, phrase: Phrase): PhraseType = {
    var `type`: Option[PhraseType] = tryParseType(phrase)
    if (`type`.isDefined) return `type`.get
    var closestPhraseOnLeft = parseResult.findClosestPhraseOnLeft(phrase)
    while ( {
      closestPhraseOnLeft.isDefined
    }) if (isVoidPhrase(closestPhraseOnLeft.get)) closestPhraseOnLeft = parseResult.findClosestPhraseOnRight(closestPhraseOnLeft.get)
    else {
      `type` = tryParseType(closestPhraseOnLeft.get)
      closestPhraseOnLeft = None
    }
    if (`type`.isDefined) return `type`.get
    var closestPhraseAbove = parseResult.findClosestPhraseAbove(phrase)
    while ( {
      closestPhraseAbove.isDefined
    }) if (isVoidPhrase(closestPhraseAbove.get)) closestPhraseAbove = parseResult.findClosestPhraseAbove(closestPhraseAbove.get)
    else {
      `type` = tryParseType(closestPhraseAbove.get)
      closestPhraseAbove = None
    }
    `type`.get
  }

  private def tryParseType(phrase: Phrase): Option[PhraseType] = {
    try
      Some(phraseTypesStore.findType(SupportedLocales.ITALY, ISSUE_DATE, phrase.text))
    catch {
      case _: IllegalArgumentException =>
        None
    }
  }


  def isValueAllowed(value: Any): Boolean = value.asInstanceOf[Date].before(new Date)

  def parseValue(raw: String): Any = {
    if (raw == null) return null
    var replaced = raw.replaceAll("-", "/")
    replaced = replaced.replaceAll("""\s""", "/")
    replaced = replaced.replaceAll("""\.""", "/")
    try {
      var df: SimpleDateFormat = null
      if (raw.length == 10) df = new SimpleDateFormat("dd/MM/yyyy")
      else if (raw.length == 8) df = new SimpleDateFormat("dd/MM/yy")
      else throw new IllegalArgumentException("Unsupported date format: '" + raw + "'")
      val ret = df.parse(replaced)
      ret
    } catch {
      case pe: ParseException =>
        throw new IllegalArgumentException(pe)
    }
  }

  def getType = ISSUE_DATE

}
