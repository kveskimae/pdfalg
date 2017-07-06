package finder.et

import dictionary._
import finder.{AbstractFinderTest, AbstractInvoiceFileReader}
import org.pdfextractor.algorithm.io._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import parser.PDFFileParser
import phrase.PhraseTypesStore
class EstonianTotalFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianTotalFinder: EstonianTotalFinder = _

  "Estonian total finder" should "find from real invoice and have additional info present" in {
    val inputStream = getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianTotalFinder.findCandidates(parseResult)

    assert(candidates.nonEmpty)

    val firstCandidate = candidates.head

    assert(firstCandidate.value != null)
    assert(firstCandidate.x != null)
    assert(firstCandidate.y != null)
    assert(firstCandidate.value == 16.87d)
    assert(35 == firstCandidate.x)
    assert(414 == firstCandidate.y)
    assert(firstCandidate.properties.get(DOUBLE_NUMBER).get.asInstanceOf[Boolean])
    assert(!firstCandidate.properties.get(EURO_SIGN_FOUND).get.asInstanceOf[Boolean])
  }

}