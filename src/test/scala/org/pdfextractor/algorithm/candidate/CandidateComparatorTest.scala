package org.pdfextractor.algorithm.candidate

import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.SupportedLocales
import org.scalatest._

import scala.collection.immutable.TreeSet

class CandidateComparatorTest  extends FlatSpec with Matchers {

  "A CandidateComparator" should "prefer a candidate in an earlier page" in {


    val candidateOnPage1: Candidate = new Candidate("1", SupportedLocales.ESTONIA, INVOICE_ID, Map(PageNo -> 1))
    val candidateOnPage2: Candidate = new Candidate("2", SupportedLocales.ESTONIA, INVOICE_ID, Map(PageNo -> 2))
    val candidateOnPage3: Candidate = new Candidate("3", SupportedLocales.ESTONIA, INVOICE_ID, Map(PageNo -> 3))
    val candidates: TreeSet[Candidate] = TreeSet(candidateOnPage3, candidateOnPage1, candidateOnPage2)

    assert(candidateOnPage1 eq candidates.head)
  }

}
