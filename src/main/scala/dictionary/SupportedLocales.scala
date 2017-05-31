package dictionary

import java.util.Locale

object SupportedLocales {

  val ESTONIAN_LANG_CODE = "et"

  val ITALIAN_LANG_CODE = "it"

  val ESTONIA: Locale = new Locale.Builder().setLanguage(ESTONIAN_LANG_CODE).setRegion("EE").build()

  val ITALY: Locale = Locale.ITALY

  def findLocaleByLanguage(language: String): Locale = language match {
    case ESTONIAN_LANG_CODE => ESTONIA
    case ITALIAN_LANG_CODE => ITALY
    case _ => throw new IllegalArgumentException("Unknown locale language: " + language)
  }

}