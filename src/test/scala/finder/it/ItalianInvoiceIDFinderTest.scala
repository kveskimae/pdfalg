package finder.it

import finder.AbstractFinderTest
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import phrase.PhraseTypesStore
import regex.RegexUtils

class ItalianInvoiceIDFinderTest extends AbstractFinderTest {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired var italianInvoiceIDFinder: ItalianInvoiceIDFinder = _

  "Italian invoice ID finder" should "parse" in {
    val idText = "Numero fattura: 3816442625428252-20"
    val parsed = italianInvoiceIDFinder.parseValue(idText).asInstanceOf[String]
    assert("3816442625428252-20" == parsed)
  }

  "Italian invoice ID finder" should "find from start" in {
    assert(RegexUtils.patternExistsInText ("Fattura n.6 del 23.02.2016", italianInvoiceIDFinder.getSearchPattern) )
  }

  "Italian invoice ID finder" should "find from line" in {
    assert(RegexUtils.patternExistsInText ("Fattura n.6 del 23.02.2016", italianInvoiceIDFinder.getValuePattern) )
    assert("Fattura n.6" == RegexUtils.findMFirstMatch ("Fattura n.6 del 23.02.2016", italianInvoiceIDFinder.getValuePattern) )
    assert("Fattura n.654343-3s" == RegexUtils.findMFirstMatch ("Fattura n.654343-3s del 23.02.2016", italianInvoiceIDFinder.getValuePattern) )
    assert("654343-3s" == RegexUtils.removeFirstOccurrence ("Fattura n.654343-3s", italianInvoiceIDFinder.PATTERN_ITALIAN_INVOICE_ID_START_PART) )
  }

}