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
import parser.{PDFFileParser, ParseResult}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

@ContextConfiguration(classes=Array(classOf[JpaConfig], classOf[StandaloneDataConfig], classOf[ExtractorConfig]))
@ActiveProfiles(Array("unittest"))
class EstonianAccountNumberFinderTest extends FlatSpec with Matchers {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var finderFactory: FinderFactory = _

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = _

  new TestContextManager(this.getClass()).prepareTestInstance(this)

  "An Estonian account number finder" should "find accounts from real PDF" in {
    val inputStream = IOHelper.getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianAccountNumberFinder.findCandidates(parseResult)
    assert(!candidates.isEmpty)
    assert(4 == candidates.size)
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])
    assert(foundValues.contains("EE882200001180000796"))
    assert(foundValues.contains("EE921010002046022001"))
    assert(foundValues.contains("EE103300332097940003"))
    assert(foundValues.contains("EE561700017000030979"))
  }

  "An Estonian account number finder" should "search accounts from the invoice string" in {
    val invoiceAsString = IOHelper.getStringFromFile("EestiEnergia.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])
    assert(!foundValues.isEmpty)
    assert(foundValues.contains("EE232200001180005555"))
    assert(foundValues.contains("EE081010002059413005"))
    assert(foundValues.contains("EE703300332099000006"))
    assert(foundValues.contains("EE431700017000115797"))
  }

  "An Estonian account number finder" should "not find incorrect accounts" in {
    val invoiceAsString = IOHelper.getStringFromFile("RiggedInvoice.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    assert(candidates.isEmpty)
  }

}
