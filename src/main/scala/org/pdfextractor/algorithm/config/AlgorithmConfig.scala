package org.pdfextractor.algorithm.config

import org.pdfextractor.algorithm.finder.FinderPackageMarker
import org.pdfextractor.algorithm.phrase.PhrasePackageMarker
import org.springframework.context.annotation.{ComponentScan, Configuration}

@Configuration
@ComponentScan(
  basePackageClasses =
    Array(classOf[FinderPackageMarker], classOf[PhrasePackageMarker]))
class AlgorithmConfig {}
