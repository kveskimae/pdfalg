package org.pdfextractor.algorithm.phrase

import java.util.Locale
import java.util.concurrent.locks.{Lock, ReentrantLock}

import org.pdfextractor.algorithm.regex._
import org.pdfextractor.db.dao.PhraseTypeDao
import org.pdfextractor.db.domain.PhraseType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.pdfextractor.db.domain.dictionary.{PaymentFieldType, SupportedLocales}
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.ApplicationContext
import org.springframework.context.event.ContextRefreshedEvent
import org.springframework.stereotype.Service

import scala.collection.{JavaConverters, mutable}

@Service
class PhraseTypesStore() {

  val log: Logger = LoggerFactory.getLogger(classOf[PhraseTypesStore])

  @Autowired private var applicationContext: ApplicationContext = _

  @Autowired private var phraseTypeDao: PhraseTypeDao = _

  val lock: Lock = new ReentrantLock

  val typesMap: collection.mutable.Map[Locale, collection.mutable.Map[PaymentFieldType, collection.mutable.ListBuffer[PhraseType]]] = collection.mutable.Map.empty[Locale, collection.mutable.Map[PaymentFieldType, collection.mutable.ListBuffer[PhraseType]]]

  @org.springframework.context.event.EventListener(Array(classOf[ContextRefreshedEvent]))
  def refreshed(): PhraseTypesRefreshedEvent = {
    lock.lock()
    try {
      log.info("Refreshing phrase types store started")
      val phraseTypes = JavaConverters.asScalaBuffer(phraseTypeDao.findAll)
      typesMap.clear()
      for (phraseType: PhraseType <- phraseTypes) {
        val locale: Locale = SupportedLocales.findLocaleByLanguage(phraseType.getLocale)
        val localeMapOp: Option[collection.mutable.Map[PaymentFieldType, collection.mutable.ListBuffer[PhraseType]]] = typesMap.get(locale)
        val localeMap: collection.mutable.Map[PaymentFieldType, collection.mutable.ListBuffer[PhraseType]] = localeMapOp.getOrElse(collection.mutable.Map.empty)
        if (!typesMap.contains(locale)) {
          typesMap.put(locale, localeMap)
        }
        val fieldTypeListOp: Option[collection.mutable.ListBuffer[PhraseType]] = localeMap.get(phraseType.getPaymentFieldType)
        val fieldTypeList: collection.mutable.ListBuffer[PhraseType] = fieldTypeListOp.getOrElse(new collection.mutable.ListBuffer[PhraseType])
        fieldTypeList += phraseType
        localeMap.put(phraseType.getPaymentFieldType, fieldTypeList)
      }
      log.info("Refreshing phrase types store completed")
    } finally {
      lock.unlock()
    }
    new PhraseTypesRefreshedEvent(applicationContext)
  }

  def findType(locale: Locale, paymentFieldType: PaymentFieldType, s: String): PhraseType = {
    getPhraseTypes(locale, paymentFieldType).
      filter(_.getPattern.matcher(s).matches).
      head
  }

  private def getPhraseTypes(locale: Locale, paymentFieldType: PaymentFieldType): collection.mutable.Seq[PhraseType] = {
    Option(locale).orElse(throw new NullPointerException)
    lock.lock()
    try {
      val fieldType2Phrase: Option[collection.mutable.Map[PaymentFieldType, collection.mutable.ListBuffer[PhraseType]]] = typesMap.get(locale)
      if (fieldType2Phrase.isEmpty) {
        throw new IllegalArgumentException("Unsupported locale: " + locale)
      }
      val phraseTypesOp: Option[collection.mutable.Seq[PhraseType]] = fieldType2Phrase.get.get(paymentFieldType)
      if (phraseTypesOp.isEmpty) {
        throw new IllegalArgumentException("Locale " + locale + " does not support field type: " + paymentFieldType)
      }
      val phraseTypes: collection.mutable.Seq[PhraseType] = phraseTypesOp.get
      if (phraseTypes.isEmpty) {
        throw new IllegalStateException("Phrase types list is emtpy for " + locale + " / " + paymentFieldType)
      }
      phraseTypes
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
              ret.append(it.next.getKeyPhrase).append("(-saateleht)?").append(OptionalWhitespace).append("(nr|number)")
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
              val abbrevation: String = it.next.getKeyPhrase
              ret.append("""([\s]{0,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{1,})""")
              ret.append(EstNameLetter).append("{4,}")
              ret.append('|')
              ret.append(EstNameLetter).append("{4,}")
              ret.append("""([\s]{1,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{0,})""")
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
              ret.append(it.next.getKeyPhrase)
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
              val kp: String = it.next.getKeyPhrase
              if (!("Invoice Number".equalsIgnoreCase(kp))) {
                // TODO Add properties metadata option for PhraseType!
                ret.append(kp).append(OptionalWhitespace).append(ItInvoiceIDWord)
                ret.append('|')
                ret.append(ItInvoiceIDWord).append(OptionalWhitespace).append(kp)
              }
              else {
                ret.append(kp).append(OptionalWhitespace)
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
              val abbrevation: String = it.next.getKeyPhrase
              ret.append("""([\s]{0,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{1,})""")
              ret.append(ItNameLetter).append("{" + ItNameMinNoLetters + ",}")
              ret.append("""([,][\s].*)?""")
              ret.append('|')
              ret.append(ItNameLetter).append("{" + ItNameMinNoLetters + ",}")
              ret.append("""([\s]{1,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{0,})""")
              ret.append("""([,][\s].*)?""")
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
              ret.append(it.next.getKeyPhrase)
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
              val abbrevation: String = it.next.getKeyPhrase
              ret.append("""([\s]{0,})(Saaja[:]?)?([\s]{0,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{1,})""")
              ret.append(EstNameLetter).append("{4,}")
              ret.append("""([\s]{3,}.*|[,].*)?""")
              ret.append('|')
              ret.append("""([\s]{0,})(Saaja[:]?)?([\s]{0,})""")
              ret.append(EstNameLetter).append("{4,}")
              ret.append("""([\s]{1,})""")
              ret.append(abbrevation)
              ret.append("""([\s]{0,})""")
              ret.append("""([\s]{3,}.*|[,].*)?""")
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
              val abbreviation: String = it.next.getKeyPhrase
              ret.append(abbreviation)
              ret.append("""([\s]{1,})""")
              ret.append(ItNameLetter).append("{4,}")
              ret.append('|')
              ret.append(ItNameLetter).append("{4,}")
              ret.append("""([\s]{1,})""")
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