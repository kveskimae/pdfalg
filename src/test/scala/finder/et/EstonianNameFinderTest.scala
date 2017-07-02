package finder.et

import finder.AbstractFinderTest
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import parser.{ParseResult, Phrase}
import phrase.PhraseTypesStore

import scala.collection.LinearSeq

class EstonianNameFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var estonianNameFinder: EstonianNameFinder = _

  "Estonian name finder" should "determine if word 'pank' is present in name" in {
    assert(EstonianNameFinder.isPankPresent("AS SEB Pank"))
    assert(!EstonianNameFinder.isPankPresent("Region Invest OÜ"))
  }

  "Estonian name finder" should "discard in a middle of a sentence" in {
    verifyFindings("Kindlustusmakses sisaldub Optimal Kindlustusmaakler OÜ vahendustasu.")
  }

  "Estonian name finder" should "discard in sentences" in {
    verifyFindings("Previous line\nPank AS LHV Pank Käibemaks\nNext line")
  }

  // OÜ

  "Estonian name finder" should "find starting OÜ" in {
    verifyFindings("OÜ Testi Firma", "OÜ Testi Firma")
  }

  "Estonian name finder" should "find ending OÜ" in {
    verifyFindings("Testi Firma OÜ", "Testi Firma OÜ")
  }

  "Estonian name finder" should "find ending OÜ and address" in {
    verifyFindings("OÜ Testi Firma, Aia 5", "OÜ Testi Firma")
  }

  // AS

  "Estonian name finder" should "find starting AS" in {
    verifyFindings("AS Eesti Telekom", "AS Eesti Telekom")
  }

  "Estonian name finder" should "find starting AS and ending reg code" in {
    verifyFindings("AS Eesti Telekom, Registrikood 10234957", "AS Eesti Telekom")
  }

  "Estonian name finder" should "find ending AS" in {
    verifyFindings("Eesti Energia AS", "Eesti Energia AS")
  }

  "Estonian name finder" should "find name ending AS and address (case 1)" in {
    verifyFindings("Eesti Energia AS, Lelle tänav 22, 11318 Tallinna linn Harju maakond", "Eesti Energia AS")
  }

  "Estonian name finder" should "find name ending AS and address (case 2)" in {
    verifyFindings("Virone Reisibüroo AS, Kaubamaja 6, 10143 Tallinn, Estonia.", "Virone Reisibüroo AS")
  }

  def verifyFindings(text: String, expected: String): Unit = {
    assume(text.nonEmpty)
    verifyFindings(text, Option(expected))
  }

  def verifyFindings(text: String, expected: Option[String] = Option.empty): Unit = {
    val list = LinearSeq(new Phrase(1, 1, 1, 1, 1, text, false))
    val candidates = estonianNameFinder.findCandidates(new ParseResult("", list))
    if (expected.nonEmpty) {
      assert(1 == candidates.size)
      assert(expected.get == candidates.head.value)
    }
    else {
      assert(candidates.isEmpty)
    }
  }

}