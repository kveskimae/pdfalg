package org.pdfextractor.algorithm.finder.it

import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.TOTAL
import org.springframework.stereotype.Service

@Service
class ItalianTotalFinder extends AbstractItalianTotalFinder(TOTAL) {}
