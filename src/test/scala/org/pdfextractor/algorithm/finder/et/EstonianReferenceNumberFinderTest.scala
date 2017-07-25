package org.pdfextractor.algorithm.finder.et

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.algorithm.finder.{AbstractFinderTest, AbstractInvoiceFileReader}
import org.pdfextractor.algorithm.io._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.pdfextractor.algorithm.parser.{PDFFileParser, ParseResult, Phrase}
import org.pdfextractor.algorithm.phrase.PhraseTypesStore
import org.pdfextractor.algorithm.finder._

import scala.collection.LinearSeq

class EstonianReferenceNumberFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianReferenceNumberFinder: EstonianReferenceNumberFinder = _

  "Tens multiple finder"  should "arrive at correct results" in {
    assert(findTensMultiple(46) == 50)
    assert(findTensMultiple(9) == 10)
    assert(findTensMultiple(21) == 30)
    assert(findTensMultiple(7) == 10)
    assert(findTensMultiple(140) == 140)
    assert(findTensMultiple(49) == 50)
    assert(findTensMultiple(63) == 70)
  }

  "731 sum calculator"  should "arrive at correct results" in {
    assert(calculate731Sum(List(1)) == 7)
    assert(calculate731Sum(List(2)) == 14)
    assert(calculate731Sum(List(3)) == 21)
    assert(calculate731Sum(List(5)) == 35)
    assert(calculate731Sum(List(6)) == 42)
    assert(calculate731Sum(List(7)) == 49)
    assert(calculate731Sum(List(8)) == 56)
    assert(calculate731Sum(List(9)) == 63)
    assert(calculate731Sum(List(1, 0, 2)) == 9)
    assert(calculate731Sum(List(4, 3, 2, 1)) == 46)
    assert(calculate731Sum(List(5, 4, 9, 6, 4, 3, 1, 6, 2)) == 140)
  }

  "Digits in rev order finder"  should "arrive at correct results" in {
    assert(digitsToPenultimateInReverse(BigInt(12345)) == Seq(4, 3, 2, 1))
    assert(digitsToPenultimateInReverse(BigInt(31)) == Seq(3))
    assert(digitsToPenultimateInReverse(BigInt(2613469450L)) == Seq(5, 4, 9, 6, 4, 3, 1, 6, 2))
  }

  "Estonian ref number finder" should "find from phrases" in {
    val invoiceAsString = getStringFromFile("EestiEnergia.txt")
    val phrases: LinearSeq[Phrase] = LinearSeq(new Phrase(1, 1, 1, 1, 1, invoiceAsString, false))
    val parseResult: ParseResult = new ParseResult("", phrases)
    val candidates: Seq[Candidate] = estonianReferenceNumberFinder.findCandidates(parseResult)
    assert(candidates.nonEmpty)
    val first: String = candidates.head.value.asInstanceOf[String]
    assert(first == "02613469450")
  }

  "Estonian ref number finder" should "not find invalid" in {
    val invoiceAsString = getStringFromFile("RiggedInvoice.txt")
    val candidates: Seq[Candidate] = estonianReferenceNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    assert(candidates.isEmpty)
  }

  "Estonian invoice ID finder" should "leave positional info" in {
    val inputStream = getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianReferenceNumberFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)
    assert(candidates.size == 1)

    val firstCandidate = candidates.head

    assert(Option(firstCandidate.value).isDefined)
    assert(Option(firstCandidate.x).isDefined)
    assert(Option(firstCandidate.y).isDefined)
    assert(firstCandidate.value == "8583082")
    assert(425 == firstCandidate.x)
    assert(94 == firstCandidate.y)
  }

}