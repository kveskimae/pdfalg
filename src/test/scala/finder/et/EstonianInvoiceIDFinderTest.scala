package finder.et

import candidate.Candidate
import config.ExtractorConfig
import finder.{AbstractInvoiceFileReader, FinderFactory}
import io.IOHelper
import org.pdfextractor.db.config.{JpaConfig, StandaloneDataConfig}
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.{ActiveProfiles, ContextConfiguration, TestContextManager}
import parser.{PDFFileParser, ParseResult, Phrase}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

@ContextConfiguration(classes = Array(classOf[JpaConfig], classOf[StandaloneDataConfig], classOf[ExtractorConfig]))
@ActiveProfiles(Array("unittest"))
class EstonianInvoiceIDFinderTest extends FlatSpec with Matchers {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var finderFactory: FinderFactory = _

  @Autowired var estonianInvoiceIDFinder: EstonianInvoiceIDFinder = _

  new TestContextManager(this.getClass()).prepareTestInstance(this)

  "An Estonian invoice ID finder" should "find incoice ID from long text" in {
    val invoiceAsString = IOHelper.getStringFromFile("EestiEnergia.txt")
    val phrase: Phrase = new Phrase(1, 1, 1, 1, 1, invoiceAsString, false)
    val phrases: LinearSeq[Phrase] = LinearSeq(phrase)
    val parseResult: ParseResult = new ParseResult("", phrases)

    val candidates: Seq[Candidate] = estonianInvoiceIDFinder.findCandidates(parseResult)

    assert(!candidates.isEmpty)

    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])

    assert(foundValues.head == "Arve nr 12345")
  }

  "An Estonian invoice ID finder" should "find ivnoice ID from real PDF" in {
    val inputStream = IOHelper.getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianInvoiceIDFinder.findCandidates(parseResult)

    assert(!candidates.isEmpty)
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