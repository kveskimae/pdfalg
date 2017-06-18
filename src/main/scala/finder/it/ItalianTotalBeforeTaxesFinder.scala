package finder.it

import dictionary.{PaymentFieldType, TOTAL_BEFORE_TAXES}
import phrase.PhraseTypesStore

class ItalianTotalBeforeTaxesFinder(phraseTypesStore: PhraseTypesStore) extends AbstractItalianTotalFinder(phraseTypesStore) {
  override def getType: PaymentFieldType = TOTAL_BEFORE_TAXES
}
