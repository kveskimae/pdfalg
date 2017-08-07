package org.pdfextractor.algorithm.finder.it

import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

import org.pdfextractor.algorithm.candidate.{Candidate, CandidateMetadata, MetaPhraseType}
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.ISSUE_DATE
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

@Service
class ItalianIssueDateFinder extends AbstractFinder(ItDateR, ItDateR, false) {

  def buildCandidate(phrase: Phrase,
                     value: Any,
                     params: Any*): Candidate = {
    val phraseType: PhraseType = params(0).asInstanceOf[PhraseType]
    val properties: Map[CandidateMetadata, Any] = Map(MetaPhraseType -> phraseType)
    new Candidate(value,
      phrase.x,
      phrase.y,
      phrase.bold,
      phrase.height,
      phrase.pageNumber,
      SupportedLocales.ITALY,
      ISSUE_DATE,
      properties)
  }

  override def findParams(phrase: Phrase, parseResult: ParseResult): Array[Any] = {
    Array(findType(parseResult, phrase))
  }

  private def findType(parseResult: ParseResult, phrase: Phrase): PhraseType = {
    tryParseType(phrase) match {
      case Some(ret) => ret
      case _ =>
        var closestPhraseOnLeft = parseResult.findClosestPhraseOnLeft(phrase)
        var maybePhraseType: Option[PhraseType] = None
        while (closestPhraseOnLeft.isDefined) {
          if (isVoidPhrase(closestPhraseOnLeft.get)) {
            closestPhraseOnLeft =
              parseResult.findClosestPhraseOnRight(closestPhraseOnLeft.get)
          } else {
            maybePhraseType = tryParseType(closestPhraseOnLeft.get)
            closestPhraseOnLeft = None
          }
        }

        if (maybePhraseType.isDefined) return maybePhraseType.get
        var closestPhraseAbove = parseResult.findClosestPhraseAbove(phrase)
        while ( {
          closestPhraseAbove.isDefined
        }) if (isVoidPhrase(closestPhraseAbove.get))
          closestPhraseAbove =
            parseResult.findClosestPhraseAbove(closestPhraseAbove.get)
        else {
          maybePhraseType = tryParseType(closestPhraseAbove.get)
          closestPhraseAbove = None
        }
        maybePhraseType.get
    }
  }

  private def tryParseType(phrase: Phrase): Option[PhraseType] = {
    try Some(
      phraseTypesStore
        .findType(SupportedLocales.ITALY, ISSUE_DATE, phrase.text))
    catch {
      case _: IllegalArgumentException =>
        None
    }
  }

  def isValueAllowed(value: Any): Boolean =
    value.asInstanceOf[Date].before(new Date)

  def parseValue(raw: String): Any = {
    val df: SimpleDateFormat = raw.length match {
      case 8 => new SimpleDateFormat("dd/MM/yy")
      case 10 => new SimpleDateFormat("dd/MM/yyyy")
      case _ =>
        throw new IllegalArgumentException(
          "Unsupported date format: '" + raw + "'")
    }

    try {
      df.parse(
        raw
          .replaceAll("-", "/")
          .replaceAll("""\s""", "/")
          .replaceAll("""\.""", "/")
      )
    } catch {
      case pe: ParseException =>
        throw new IllegalArgumentException(pe)
    }
  }

  def getType = ISSUE_DATE

}
