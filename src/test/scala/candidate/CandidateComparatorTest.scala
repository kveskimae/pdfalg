package candidate

import org.junit.runner.RunWith
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.TreeSet

@RunWith(classOf[JUnitRunner])
class CandidateComparatorTest  extends FlatSpec with Matchers {

  "A CandidateComparator" should "prefer a candidate in an earlier page" in {
    val candidateOnPage1: Candidate = new Candidate("1", 1, 1, false, 1, 1, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
    val candidateOnPage2: Candidate = new Candidate("2", 1, 1, false, 1, 2, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
    val candidateOnPage3: Candidate = new Candidate("3", 1, 1, false, 1, 3, SupportedLocales.ESTONIA, INVOICE_ID, Map.empty)
    val candidates: TreeSet[Candidate] = TreeSet(candidateOnPage3, candidateOnPage1, candidateOnPage2)

    assert(candidateOnPage1 eq candidates.head)
  }

}
