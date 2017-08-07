package org.pdfextractor.algorithm.finder.it

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.candidate.{Candidate, CandidateMetadata, MetaPhraseType}
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.algorithm.parser.Phrase
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

@Service
class ItalianNameFinder extends AbstractFinder(None, None, false) {

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY,
        NAME) + "$").r)
    valuePattern = Some(("^(?ims)(.*)$").r)
  }

  override def buildCandidate(phrase: Phrase,
                              value: Any,
                              params: Any*): Candidate = {
    val `type` =
      phraseTypesStore.findType(SupportedLocales.ITALY, NAME, phrase.text)
    val properties: Map[CandidateMetadata, Any] = Map(MetaPhraseType -> `type`)
    val ret = new Candidate(value,
      phrase.x,
      phrase.y,
      phrase.bold,
      phrase.height,
      phrase.pageNumber,
      SupportedLocales.ITALY,
      NAME,
      properties)
    ret
  }

  def isValueAllowed(value: Any): Boolean = {
    !isVoidText(value.asInstanceOf[String]) &&
      ItNameForbiddenWordsR.findFirstIn(value.asInstanceOf[String]).isEmpty &&
      ItNameMinR.findFirstIn(value.asInstanceOf[String]).nonEmpty
  }

  def parseValue(raw: String): Any = {
    if (Option(raw).isEmpty) None
    else StringUtils.normalizeSpace(raw).split(",")(0)
  }

  def getType = NAME

}
