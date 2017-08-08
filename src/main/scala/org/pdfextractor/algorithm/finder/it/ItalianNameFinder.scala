package org.pdfextractor.algorithm.finder.it

import java.util.Locale

import org.apache.commons.lang3.StringUtils
import org.pdfextractor.algorithm.candidate.{CandidateMetadata, MetaPhraseType}
import org.pdfextractor.algorithm.finder._
import org.pdfextractor.algorithm.finder.it.ItalianRegexPatterns._
import org.pdfextractor.algorithm.parser.{ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesRefreshedEvent
import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.NAME
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.springframework.stereotype.Service

@Service
class ItalianNameFinder extends AbstractFinder(SupportedLocales.ITALY, NAME, None, None, false) {

  @org.springframework.context.event.EventListener(
    Array(classOf[PhraseTypesRefreshedEvent]))
  def refreshed(): Unit = {
    searchPattern = Some(
      ("^(?ims)" + phraseTypesStore.buildAllPhrases(SupportedLocales.ITALY,
        NAME) + "$").r)
    valuePattern = Some(("^(?ims)(.*)$").r)
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

  override def buildProperties(phrase: Phrase, parseResult: ParseResult, params: Seq[Any]): Map[CandidateMetadata, Any] = {
    val phraseType = phraseTypesStore.findType(SupportedLocales.ITALY, NAME, phrase.text)
    Map(MetaPhraseType -> phraseType)
  }

}
