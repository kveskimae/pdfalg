package phrase

import java.util.Locale
import java.util.concurrent.locks.{Lock, ReentrantLock}

import candidate.PhraseType
import dictionary._
import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.slf4j.{Logger, LoggerFactory}
import regex.CommonRegexPatterns
import regex.CommonRegexPatterns._

import scala.collection.mutable

class PhraseTypesStore() {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  val lock: Lock = new ReentrantLock

  val typesMap: collection.mutable.Map[Locale, collection.mutable.Map[PaymentFieldType, collection.mutable.Seq[PhraseType]]] = collection.mutable.Map.empty[Locale, collection.mutable.Map[PaymentFieldType, collection.mutable.Seq[PhraseType]]]

  def refreshed(phraseTypes: Seq[PhraseType]): Unit = {
    lock.lock()
    try {
      log.info("Refreshing phrase types store started")
      typesMap.clear()
      for (phraseType <- phraseTypes) {
        val locale: Locale = SupportedLocales.findLocaleByLanguage(phraseType.locale)
        val localeMapOp: Option[collection.mutable.Map[PaymentFieldType, collection.mutable.Seq[PhraseType]]] = typesMap.get(locale)
        val localeMap: collection.mutable.Map[PaymentFieldType, collection.mutable.Seq[PhraseType]] = localeMapOp.getOrElse(collection.mutable.Map.empty)
        if (localeMapOp.isEmpty) {
          typesMap.put(locale, localeMap)
        }
        val fieldTypeListOp: Option[collection.mutable.Seq[PhraseType]] = localeMap.get(phraseType.paymentFieldType)
        val fieldTypeList: collection.mutable.Seq[PhraseType] = fieldTypeListOp.getOrElse(collection.mutable.Seq.empty[PhraseType])
        if (fieldTypeListOp.isEmpty) {
          localeMap.put(phraseType.paymentFieldType, fieldTypeList)
        }
        fieldTypeList :+ phraseType
      }
      log.info("Refreshing phrase types store completed")
    } finally {
      lock.unlock()
    }
    // TODO return PhraseTypesRefreshedEvent
  }

  def findType(locale: Locale, paymentFieldType: PaymentFieldType, s: String): PhraseType = {
    val phraseTypes: mutable.Seq[PhraseType] = getPhraseTypes(locale, paymentFieldType)
    for (phraseType <- phraseTypes) {
      if (phraseType.pattern.matcher(s).matches) {
        return phraseType
      }
    }
    throw new IllegalArgumentException("Cannot find a matching phrase: '" + s + "'")
  }

  private def getPhraseTypes(locale: Locale, paymentFieldType: PaymentFieldType): collection.mutable.Seq[PhraseType] = {
    if (locale == null) {
      throw new NullPointerException("Parameter locale is null")
    }
    lock.lock()
    try {
      if (typesMap.get(locale) == null) {
        throw new IllegalArgumentException("Unsupported locale: " + locale)
      }
      if (typesMap.get(locale).get(paymentFieldType) == null) {
        throw new IllegalArgumentException("Locale " + locale + " does not support field type: " + paymentFieldType)
      }
      typesMap.get(locale).get(paymentFieldType)
    } finally {
      lock.unlock()
    }
  }

  def buildAllPhrases(locale: Locale, paymentFieldType: PaymentFieldType): String = {
    val ret: StringBuilder = new StringBuilder
    val it: Iterator[PhraseType] = getPhraseTypes(locale, paymentFieldType).iterator
    locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        paymentFieldType match {
          case INVOICE_ID =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              ret.append(it.next.keyPhrase).append("(-saateleht)?").append(OPTIONAL_WHITESPACE).append("(nr|number)")
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
          case NAME =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              val abbrevation: String = it.next.keyPhrase
              ret.append("([\\s]{0,})")
              ret.append(abbrevation)
              ret.append("([\\s]{1,})")
              ret.append(CommonRegexPatterns.ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              // ret.append("([,][\\s].*)?");
              ret.append('|')
              ret.append(CommonRegexPatterns.ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              ret.append("([\\s]{1,})")
              ret.append(abbrevation)
              ret.append("([\\s]{0,})")
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case TOTAL =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              ret.append(it.next.keyPhrase)
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case _ =>
            throw new IllegalArgumentException("Unsupported field type: " + paymentFieldType)
        }
        
      case SupportedLocales.ITALIAN_LANG_CODE =>
        paymentFieldType match {
          case INVOICE_ID =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              val kp: String = it.next.keyPhrase
              if (!("Invoice Number".equalsIgnoreCase(kp))) {
                // TODO Add properties metadata option for PhraseType!
                ret.append(kp).append(OPTIONAL_WHITESPACE).append(ITALIAN_INVOICE_ID_WORDS)
                ret.append('|')
                ret.append(ITALIAN_INVOICE_ID_WORDS).append(OPTIONAL_WHITESPACE).append(kp)
              }
              else {
                ret.append(kp).append(OPTIONAL_WHITESPACE)
              }
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case NAME =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              val abbrevation: String = it.next.keyPhrase
              ret.append("([\\s]{0,})")
              ret.append(abbrevation)
              ret.append("([\\s]{1,})")
              ret.append(CommonRegexPatterns.ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}")
              ret.append("([,][\\s].*)?")
              ret.append('|')
              ret.append(CommonRegexPatterns.ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{" + MINIMUM_NUMBER_OF_CHARACTERS + ",}")
              ret.append("([\\s]{1,})")
              ret.append(abbrevation)
              ret.append("([\\s]{0,})")
              ret.append("([,][\\s].*)?")
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case TOTAL =>
          case TOTAL_BEFORE_TAXES =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              ret.append(it.next.keyPhrase)
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case _ =>
            throw new IllegalArgumentException("Unsupported field type: " + paymentFieldType)
        }
        
      case _ =>
        throw new IllegalArgumentException("Unsupported locale: " + locale)
    }
    return ret.toString
  }

  def buildAllStarts(locale: Locale, paymentFieldType: PaymentFieldType): String = {
    val ret: StringBuilder = new StringBuilder
    val it: Iterator[PhraseType] = getPhraseTypes(locale, paymentFieldType).iterator
    locale.getLanguage match {
      case SupportedLocales.ESTONIAN_LANG_CODE =>
        paymentFieldType match {
          case NAME =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              val abbrevation: String = it.next.keyPhrase
              ret.append("([\\s]{0,})(Saaja[:]?)?([\\s]{0,})")
              ret.append(abbrevation)
              ret.append("([\\s]{1,})")
              ret.append(CommonRegexPatterns.ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              ret.append("([\\s]{3,}.*|[,].*)?")
              ret.append('|')
              ret.append("([\\s]{0,})(Saaja[:]?)?([\\s]{0,})")
              ret.append(CommonRegexPatterns.ESTONIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              ret.append("([\\s]{1,})")
              ret.append(abbrevation)
              ret.append("([\\s]{0,})")
              ret.append("([\\s]{3,}.*|[,].*)?")
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case _ =>
            throw new IllegalArgumentException("Unsupported field type: " + paymentFieldType)
        }
        
      case SupportedLocales.ITALIAN_LANG_CODE =>
        paymentFieldType match {
          case NAME =>
            ret.append('(')
            while ( {
              it.hasNext
            }) {
              val abbreviation: String = it.next.keyPhrase
              ret.append(abbreviation)
              ret.append("([\\s]{1,})")
              ret.append(CommonRegexPatterns.ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              ret.append('|')
              ret.append(CommonRegexPatterns.ITALIAN_ALPHANUMERIC_LETTER_OR_SPACE_OR_AMPERSAND).append("{4,}")
              ret.append("([\\s]{1,})")
              ret.append(abbreviation)
              if (it.hasNext) {
                ret.append('|')
              }
            }
            ret.append(')')
            
          case _ =>
            throw new IllegalArgumentException("Unsupported field type: " + paymentFieldType)
        }
        
      case _ =>
        throw new IllegalArgumentException("Unsupported locale: " + locale)
    }
    return ret.toString
  }

}