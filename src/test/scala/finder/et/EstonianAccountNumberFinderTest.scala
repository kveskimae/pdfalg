package finder.et

import java.io.IOException

import candidate.Candidate
import config.ExtractorConfig
import finder.AbstractInvoiceFileReader
import io.IOHelper
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.pdfextractor.db.config.{JpaConfig, StandaloneDataConfig}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner
import org.springframework.test.context.{ActiveProfiles, ContextConfiguration}
import parser.{PDFFileParser, ParseResult}

import scala.collection.LinearSeq

@RunWith(classOf[SpringJUnit4ClassRunner])
@ContextConfiguration(classes=Array(classOf[JpaConfig], classOf[StandaloneDataConfig], classOf[ExtractorConfig]))
@ActiveProfiles(Array("unittest"))
class EstonianAccountNumberFinderTest extends AbstractInvoiceFileReader {

  @Autowired var estonianAccountNumberFinder: EstonianAccountNumberFinder = _

  @Test
  @throws[IOException]
  def testFindsSeveralAccountNumbers(): Unit = {
    val inputStream = IOHelper.getInputStreamFromFile(AbstractInvoiceFileReader.Starman)
    val parseResult = PDFFileParser.parse(inputStream)
    val candidates = estonianAccountNumberFinder.findCandidates(parseResult)
    assertFalse(candidates.isEmpty)
    assertEquals(4, candidates.size)
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])
    assertTrue(foundValues.contains("EE882200001180000796"))
    assertTrue(foundValues.contains("EE921010002046022001"))
    assertTrue(foundValues.contains("EE103300332097940003"))
    assertTrue(foundValues.contains("EE561700017000030979"))
  }

  @Test
  @throws[IOException]
  def testFindingReferenceNumber(): Unit = {
    val invoiceAsString = IOHelper.getStringFromFile("EestiEnergia.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    val foundValues: Seq[String] = candidates.map(_.getValue.asInstanceOf[String])

    assertFalse(foundValues.isEmpty)

    assertTrue(foundValues.contains("EE232200001180005555"))
    assertTrue(foundValues.contains("EE081010002059413005"))
    assertTrue(foundValues.contains("EE703300332099000006"))
    assertTrue(foundValues.contains("EE431700017000115797"))
  }

  @Test
  @throws[IOException]
  def testNotFindingIncorrectReferenceNumber(): Unit = {
    val invoiceAsString = IOHelper.getStringFromFile("RiggedInvoice.txt")
    val candidates: Seq[Candidate] = estonianAccountNumberFinder.findCandidates(new ParseResult(invoiceAsString, LinearSeq.empty))
    assert(candidates.isEmpty)
  }

}
