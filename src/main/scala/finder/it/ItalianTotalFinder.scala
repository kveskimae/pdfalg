package finder.it

import dictionary.{PaymentFieldType, TOTAL}
import phrase.PhraseTypesStore

class ItalianTotalFinder(phraseTypesStore: PhraseTypesStore) extends AbstractItalianTotalFinder(phraseTypesStore) {

  override def getType: PaymentFieldType = TOTAL

}
