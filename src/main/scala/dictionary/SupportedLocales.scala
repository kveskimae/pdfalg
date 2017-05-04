package dictionary

import java.util.Locale

object SupportedLocales {

  val ESTONIAN_LANG_CODE = "et"

  val ITALIAN_LANG_CODE = "it"

  val ESTONIA: Locale = new Locale.Builder().setLanguage(ESTONIAN_LANG_CODE).setRegion("EE").build()

  val ITALY: Locale = Locale.ITALY

}