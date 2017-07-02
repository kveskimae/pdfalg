package finder.et

import candidate.Candidate
import finder.{AbstractFinderTest, AbstractInvoiceFileReader}
import io.IOHelper
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import parser.{PDFFileParser, ParseResult, Phrase}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

class EstonianReferenceNumberFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianReferenceNumberFinder: EstonianReferenceNumberFinder = _

  "Estonian ref number finder" should "find from phrases" in {
    val invoiceAsString = IOHelper.getStringFromFile("EestiEnergia.txt")
    val phrases: LinearSeq[Phrase] = LinearSeq(new Phrase(1, 1, 1, 1, 1, invoiceAsString, false))
    val parseResult: ParseResult = new ParseResult("", phrases)
    val candidates: Seq[Candidate] = estonianReferenceNumberFinder.findCandidates(parseResult)
    assert(candidates.nonEmpty)
    val first: String = candidates.head.value.asInstanceOf[String]
    assert(first == "02613469450")
  }

  "Estonian ref number finder" should "not find invalid" in {
    val invoiceAsString = IOHelper.getStringFromFile("RiggedInvoice.txt")
    val candidates: Seq[Candidate] = estonianReferenceNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    assert(candidates.isEmpty)
  }

  "Estonian invoice ID finder" should "leave positional info" in {
    val inputStream = IOHelper.getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianReferenceNumberFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)
    assert(candidates.size == 1)

    val firstCandidate = candidates.head

    assert(firstCandidate.value != null)
    assert(firstCandidate.x != null)
    assert(firstCandidate.y != null)
    assert(firstCandidate.value == "8583082")
    assert(425 == firstCandidate.x)
    assert(94 == firstCandidate.y)
  }

}