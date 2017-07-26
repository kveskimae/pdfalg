package org.pdfextractor.algorithm.phrase

import org.springframework.context.ApplicationContext
import org.springframework.context.event.ApplicationContextEvent

class PhraseTypesRefreshedEvent(val applicationContext: ApplicationContext)
    extends ApplicationContextEvent(applicationContext) {}
