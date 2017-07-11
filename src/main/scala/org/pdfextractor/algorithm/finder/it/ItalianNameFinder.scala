package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.algorithm.candidate.{Candidate, PHRASE_TYPE, PropertyType}
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent

object ItalianNameFinder {
  val MINIMUM_NUMBER_OF_CHARACTERS: Integer = 3

  private val PATTERN_MINIMUM_CHARACTERS_AS_REGEX2 = ("^(?ims).*" + ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND + "{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}.*$").r
}

@Service
class ItalianNameFinder extends AbstractFinder(None, None, false) {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY, NAME) + "$").r)
    valuePattern = Some(("^(?ims)(.*)$").r)
  }

  protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val `type` = phraseTypesStore.findType(SupportedLocales.ITALY, NAME, phrase.text)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> `type`)
    val ret = new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ITALY, NAME, properties)
    ret
  }

  def isValueAllowed(value: Any): Boolean = {
    val ret = !isVoidText(value.asInstanceOf[String]) && PATTERN_ITALIAN_NAME_FORBIDDEN_AS_REGEX.findFirstIn(value.asInstanceOf[String]).isEmpty && PATTERN_MINIMUM_CHARACTERS_AS_REGEX.findFirstIn(value.asInstanceOf[String]).nonEmpty
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
