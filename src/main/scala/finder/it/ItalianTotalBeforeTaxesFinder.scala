package finder.it

import dictionary.{PaymentFieldType, TOTAL_BEFORE_TAXES}
import org.springframework.stereotype.Service
import phrase.PhraseTypesStore

@Service
class ItalianTotalBeforeTaxesFinder(phraseTypesStore: PhraseTypesStore) extends AbstractItalianTotalFinder(phraseTypesStore) {
  override def getType: PaymentFieldType = TOTAL_BEFORE_TAXES
}
