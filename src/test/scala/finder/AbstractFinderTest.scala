package finder

import config.ExtractorConfig
import org.pdfextractor.db.config.{JpaConfig, StandaloneDataConfig}
import org.scalatest.{FlatSpec, Matchers}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.{ActiveProfiles, ContextConfiguration, TestContextManager}

@ContextConfiguration(classes = Array(classOf[JpaConfig], classOf[StandaloneDataConfig], classOf[ExtractorConfig]))
@ActiveProfiles(Array("unittest"))
class AbstractFinderTest extends FlatSpec with Matchers {

  @Autowired var finderFactory: FinderFactory = _

  new TestContextManager(getClass).prepareTestInstance(this)

}
