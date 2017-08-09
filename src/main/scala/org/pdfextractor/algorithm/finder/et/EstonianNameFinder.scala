package org.pdfextractor.algorithm.finder.et

import java.util.Locale

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.candidate.{Candidate, CandidateMetadata, HasPank, MetaPhraseType}
import org.pdfextractor.algorithm.finder.{AbstractFinder, isPankPresent}
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

@Service
class EstonianNameFinder extends AbstractFinder(SupportedLocales.ESTONIA, NAME) {

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?m)" + phraseTypesStore
        .buildAllStarts(SupportedLocales.ESTONIA, NAME) + "$").r)
    valuePattern = Some(
      ("(?m)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ESTONIA,
        NAME)).r)
  }

  override def parseValue(raw: String): Any = {
    StringUtils normalizeSpace (raw
      .replaceAll("(Registrikood)(.{0,})", "")
      .split("""[\s]{3,}""")(0))
  }

  override def buildProperties(phrase: Phrase, parseResult: ParseResult, params: Seq[Any]): Map[CandidateMetadata, Any] = {
    val phraseType = phraseTypesStore.findType(SupportedLocales.ESTONIA, NAME, phrase.text)
    val pankPresent = isPankPresent(phrase.text)
    Map(MetaPhraseType -> phraseType, HasPank -> pankPresent)
  }

}
