package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate.{Candidate, HasPank, MetaPhraseType, CandidateMetadata}
import org.pdfextractor.algorithm.finder.AbstractFinder
import org.apache.commons.lang3.StringUtils
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.finder.isPankPresent

@Service
class EstonianNameFinder extends AbstractFinder {

  @org.springframework.context.event.EventListener(Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(("^(?m)" + phraseTypesStore.buildAllStarts(SupportedLocales.ESTONIA, NAME) + "$").r)
    valuePattern = Some(("(?m)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA, NAME)).r)
  }

  private def buildProperties(phrase: Phrase): Map[CandidateMetadata, Any] = {
    val phraseType = phraseTypesStore.findType(SupportedLocales.ESTONIA, NAME, phrase.text)
    val pankPresent = isPankPresent(phrase.text)
    Map(MetaPhraseType -> phraseType, HasPank -> pankPresent)
  }

  override protected def buildCandidate(parseResult: ParseResult, phrase: Phrase, value: Any, params: Any*): Candidate = {
    new Candidate(value, phrase.x, phrase.y, phrase.bold, phrase.height, phrase.pageNumber, SupportedLocales.ESTONIA, NAME, buildProperties(phrase))
  }

  override def isValueAllowed(value: Any) = true

  override  def parseValue(raw: String): Any = {
    StringUtils normalizeSpace (raw.replaceAll("(Registrikood)(.{0,})", "").split("""[\s]{3,}""")(0))
  }

  override def getType = NAME

}
