package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.TOTAL_BEFORE_TAXES
import org.springframework.stereotype.Service

@Service
class ItalianTotalBeforeTaxesFinder extends AbstractItalianTotalFinder {

  override def getType: PaymentFieldType = TOTAL_BEFORE_TAXES

}