package finder.it

import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType.TOTAL
import org.springframework.stereotype.Service
import phrase.PhraseTypesStore

@Service
class ItalianTotalFinder extends AbstractItalianTotalFinder {

  override def getType: PaymentFieldType = TOTAL

}
