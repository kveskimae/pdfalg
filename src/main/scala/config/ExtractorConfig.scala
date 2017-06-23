package config

import finder.FinderPackageMarker
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.Configuration

@Configuration
@ComponentScan(basePackageClasses = Array(classOf[FinderPackageMarker]))
class ExtractorConfig {}