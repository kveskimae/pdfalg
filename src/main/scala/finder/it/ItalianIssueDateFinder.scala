package finder.it

import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

import candidate.{Candidate, PhraseType}
import dictionary._
import finder.AbstractFinder
import finder.it.ItalianRegexPatterns._
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore

@Service
class ItalianIssueDateFinder(phraseTypesStore: PhraseTypesStore) extends AbstractFinder(phraseTypesStore, PATTERN_ITALIAN_DATE, PATTERN_ITALIAN_DATE, false) {

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type`: PhraseType = findType(parseResult, phrase)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, ISSUE_DATE, properties)
    ret
  }

  private def findType(parseResult: ParseResult, phrase: Phrase): PhraseType = {
    var `type` = tryParseType(phrase)
    if (`type` != null) return `type`
    var closestPhraseOnLeft = parseResult.findClosestPhraseOnLeft(phrase)
    while ( {
      closestPhraseOnLeft != null
    }) if (AbstractFinder.isVoidPhrase(closestPhraseOnLeft)) closestPhraseOnLeft = parseResult.findClosestPhraseOnRight(closestPhraseOnLeft)
    else {
      `type` = tryParseType(closestPhraseOnLeft)
      closestPhraseOnLeft = null
    }
    if (`type` != null) return `type`
    var closestPhraseAbove = parseResult.findClosestPhraseAbove(phrase)
    while ( {
      closestPhraseAbove != null
    }) if (AbstractFinder.isVoidPhrase(closestPhraseAbove)) closestPhraseAbove = parseResult.findClosestPhraseAbove(closestPhraseAbove)
    else {
      `type` = tryParseType(closestPhraseAbove)
      closestPhraseAbove = null
    }
    `type`
  }

  private def tryParseType(phrase: Phrase): PhraseType = {
    try
      return phraseTypesStore.findType(SupportedLocales.ITALY, ISSUE_DATE, phrase.text)
    catch {
      case ignored: IllegalArgumentException =>
    }
    null
  }


  def isValueAllowed(value: Any): Boolean = value.asInstanceOf[Date].before(new Date)

  def parseValue(raw: String): Any = {
    if (raw == null) return null
    var replaced = raw.replaceAll("-", "/")
    replaced = replaced.replaceAll("\\s", "/")
    replaced = replaced.replaceAll("\\.", "/")
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
