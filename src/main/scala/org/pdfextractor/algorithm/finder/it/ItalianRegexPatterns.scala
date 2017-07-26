package org.pdfextractor.algorithm.finder.it

object ItalianRegexPatterns {

  val ItNameForbiddenWordsR = ("""^(?ims)(amministrazione|fattura)$""").r

  // Date

  val ItDateR =
    ("""([\d]{2,2}[\-\s./][\d]{2,2}[\-\s./]([\d]{4,4}|[\d]{2,2}))""").r

  // VATIN

  val ItVatinR = ("""^(?ims)([^\d].*)?(\d{11})([^\d].*)?$""").r

  val ItVatinValueR = ("""(\d{11})""").r

}
