package finder.et

import candidate.Candidate
import finder.{AbstractFinderTest, AbstractInvoiceFileReader}
import io.IOHelper
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import parser.{PDFFileParser, ParseResult, Phrase}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

class EstonianInvoiceIDFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianInvoiceIDFinder: EstonianInvoiceIDFinder = _

  "Estonian invoice ID finder" should "find from phrase" in {
    val invoiceAsString = IOHelper.getStringFromFile("EestiEnergia.txt")
    val phrase: Phrase = new Phrase(1, 1, 1, 1, 1, invoiceAsString, false)
    val phrases: LinearSeq[Phrase] = LinearSeq(phrase)
    val parseResult: ParseResult = new ParseResult("", phrases)

    val candidates: Seq[Candidate] = estonianInvoiceIDFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)

    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])

    assert(foundValues.head == "Arve nr 12345")
  }

  "Estonian invoice ID finder" should "find from real PDF" in {
    val inputStream = IOHelper.getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianInvoiceIDFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)
    assert(candidates.size == 1)

    val firstCandidate = candidates.head

    assert(firstCandidate.value != null)
    assert(firstCandidate.x != null)
    assert(firstCandidate.y != null)
    assert(firstCandidate.value == "Arve number A-123456")
    assert(330 == firstCandidate.x)
    assert(94 == firstCandidate.y)
  }

}