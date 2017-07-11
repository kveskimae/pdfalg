package finder.et

import org.pdfextractor.algorithm.candidate.{Candidate, ESTONIAN_IS_PANK_PRESENT, PHRASE_TYPE, PropertyType}
import finder.AbstractFinder
import finder.et.EstonianRegexPatterns._
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import phrase.PhraseTypesRefreshedEvent

object EstonianNameFinder {

  def isPankPresent(text: String): Boolean = {
    PATTERN_ESTONIAN_PANK_AS_REGEX.findFirstIn(text).nonEmpty
  }

}

@Service
class EstonianNameFinder extends AbstractFinder {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("^(?m)" + phraseTypesStore.buildAllStarts(SupportedLocales.ESTONIA, NAME) + "$").r)
    valuePattern = Some(("(?m)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, NAME)).r)
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    val phraseType = phraseTypesStore.findType(SupportedLocales.ESTONIA, NAME, phrase.text)
    val pankPresent = EstonianNameFinder.isPankPresent(phrase.text)
    val properties: Map[PropertyType, Any] = Map(PHRASE_TYPE -> phraseType, ESTONIAN_IS_PANK_PRESENT -> pankPresent)
    new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, NAME, properties)
  }

  override def isValueAllowed(value: Any) = true

  override  def parseValue(raw: String): Any = {
    var ret = raw.replaceAll("(Registrikood)(.{0,})", "")
    ret = ret.split("""[\s]{3,}""")(0)
    ret = StringUtils.normalizeSpace(ret)
    ret
  }

  override def getType = NAME

}
