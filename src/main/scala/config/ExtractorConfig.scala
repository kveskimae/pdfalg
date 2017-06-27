package config

import finder.FinderPackageMarker
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration
import phrase.PhrasePackageMarker

@Configuration
@ComponentScan(basePackageClasses = Array(classOf[FinderPackageMarker], classOf[PhrasePackageMarker]))
class ExtractorConfig {}