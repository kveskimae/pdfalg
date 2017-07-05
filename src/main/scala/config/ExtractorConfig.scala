package config

import finder.FinderPackageMarker
import org.springframework.context.annotation.{ComponentScan, Configuration}
import phrase.PhrasePackageMarker

@Configuration
@ComponentScan(basePackageClasses = Array(classOf[FinderPackageMarker], classOf[PhrasePackageMarker]))
class ExtractorConfig {}