package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate.{HasEuroSign, IsDouble}
import org.pdfextractor.algorithm.finder.AbstractFinderTest
import org.pdfextractor.algorithm.io._
import org.pdfextractor.algorithm.parser.PDFFileParser
import org.pdfextractor.algorithm.phrase.PhraseTypesStore
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
class EstonianTotalFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianTotalFinder: EstonianTotalFinder = _

  "Estonian total finder" should "find from real invoice and have additional info present" in {
    val inputStream = getInputStreamFromFile(Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianTotalFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)

    val firstCandidate = candidates.head

    assert(Option(firstCandidate.value).isDefined)
    assert(Option(firstCandidate.x).isDefined)
    assert(Option(firstCandidate.y).isDefined)
    assert(firstCandidate.value == 16.87d)
    assert(35 == firstCandidate.x)
    assert(414 == firstCandidate.y)
    assert(firstCandidate.features.get(IsDouble).get.asInstanceOf[Boolean])
    assert(!firstCandidate.features.get(HasEuroSign).get.asInstanceOf[Boolean])
  }

}