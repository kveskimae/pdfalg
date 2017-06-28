package io

import java.awt.Point

import org.pdfextractor.db.domain.dictionary.PaymentFieldType
import org.pdfextractor.db.domain.dictionary.PaymentFieldType._
import org.scalatest._

class IOHelperTest  extends FlatSpec with Matchers {

  "An IOHelper" should "find " in {
    val locationsMap: scala.collection.Map[PaymentFieldType, Seq[Point]] = IOHelper.getMapFromFile("locations.json");
    assert(locationsMap != null)
    assert(locationsMap.size > 0)
    assert(locationsMap.get(TOTAL).nonEmpty)
    assert(locationsMap(TOTAL).size > 0)
    assert(locationsMap.get(REFERENCE_NUMBER).nonEmpty)
    assert(locationsMap(REFERENCE_NUMBER).size > 0)
  }

}