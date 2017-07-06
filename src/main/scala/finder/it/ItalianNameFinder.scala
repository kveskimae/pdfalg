package finder.it

import candidate.Candidate
import dictionary._
import finder.AbstractFinder
import finder.it.ItalianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesRefreshedEvent

object ItalianNameFinder {
  val MINIMUM_NUMBER_OF_CHARACTERS: Integer = 3

  private val PATTERN_MINIMUM_CHARACTERS_AS_REGEX2 = ("^(?ims).*" + ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND + "{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}.*$").r
}

@Service
class ItalianNameFinder extends AbstractFinder(null, null, false) {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = ("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, NAME) + "$").r
    valuePattern = ("^(?ims)(.*)$").r
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type` = phraseTypesStore.findType(SupportedLocales.ITALY, NAME, phrase.text)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, NAME, properties)
    ret
  }

  def isValueAllowed(value: Any): Boolean = {
    val ret = !AbstractFinder.isVoidText(value.asInstanceOf[String]) && PATTERN_ITALIAN_NAME_FORBIDDEN_AS_REGEX.findFirstIn(value.asInstanceOf[String]).isEmpty && PATTERN_MINIMUM_CHARACTERS_AS_REGEX.findFirstIn(value.asInstanceOf[String]).nonEmpty
    ret
  }

  def parseValue(raw: String): Any = {
    if (raw == null) return null
    var ret = raw
    ret = StringUtils.normalizeSpace(ret)
    val bits = ret.split(",")
    ret = bits(0)
    ret
  }

  def getType = NAME

}
