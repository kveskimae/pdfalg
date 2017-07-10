package finder.et

import org.pdfextractor.algorithm.candidate.Candidate
import finder.{AbstractFinderTest, AbstractInvoiceFileReader}
import org.pdfextractor.algorithm.io._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import parser.{PDFFileParser, ParseResult}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

class EstonianAccountNumberFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = _

  "Estonian account finder" should "find from real PDF" in {
    val inputStream = getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianAccountNumberFinder.findCandidates(parseResult)
    assert(candidates.nonEmpty)
    assert(4 == candidates.size)
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])
    assert(foundValues.contains("EE882200001180000796"))
    assert(foundValues.contains("EE921010002046022001"))
    assert(foundValues.contains("EE103300332097940003"))
    assert(foundValues.contains("EE561700017000030979"))
  }

  "Estonian account finder" should "find from invoice as a string" in {
    val invoiceAsString = getStringFromFile("EestiEnergia.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])
    assert(foundValues.nonEmpty)
    assert(foundValues.contains("EE232200001180005555"))
    assert(foundValues.contains("EE081010002059413005"))
    assert(foundValues.contains("EE703300332099000006"))
    assert(foundValues.contains("EE431700017000115797"))
  }

  "Estonian account finder" should "discard invalid accounts" in {
    val invoiceAsString = getStringFromFile("RiggedInvoice.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    assert(candidates.isEmpty)
  }

}
