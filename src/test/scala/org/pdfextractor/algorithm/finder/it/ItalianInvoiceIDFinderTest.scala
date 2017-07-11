package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.algorithm.finder.AbstractFinderTest
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.pdfextractor.algorithm.phrase.PhraseTypesStore

class ItalianInvoiceIDFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var italianInvoiceIDFinder: ItalianInvoiceIDFinder = _

  "Italian invoice ID finder" should "parse" in {
    val idText = "Numero fattura: 3816442625428252-20"
    val parsed = italianInvoiceIDFinder.parseValue(idText).asInstanceOf[String]
    assert("3816442625428252-20" == parsed)
  }

  "Italian invoice ID finder" should "find from start" in {
    assert(italianInvoiceIDFinder.getSearchPattern.findFirstIn("Fattura n.6 del 23.02.2016").nonEmpty)
  }

  "Italian invoice ID finder" should "find from line" in {
    assert(italianInvoiceIDFinder.getValuePattern.findFirstIn("Fattura n.6 del 23.02.2016").nonEmpty)
    assert("Fattura n.6" == italianInvoiceIDFinder.getValuePattern.findFirstIn("Fattura n.6 del 23.02.2016").get)
    assert("Fattura n.654343-3s" == italianInvoiceIDFinder.getValuePattern.findFirstIn("Fattura n.654343-3s del 23.02.2016").get)
    assert("654343-3s" == italianInvoiceIDFinder.PATTERN_ITALIAN_INVOICE_ID_START_PART_AS_REGEX.replaceFirstIn("Fattura n.654343-3s", ""))
  }

}