package org.pdfextractor.algorithm.config

import org.pdfextractor.algorithm.finder.FinderPackageMarker
import org.springframework.context.annotation.{ComponentScan, Configuration}
import org.pdfextractor.algorithm.phrase.PhrasePackageMarker

@Configuration
@ComponentScan(
  basePackageClasses =
    Array(classOf[FinderPackageMarker], classOf[PhrasePackageMarker]))
class AlgorithmConfig {}
