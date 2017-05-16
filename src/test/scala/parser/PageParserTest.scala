package parser

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PageParserTest extends FunSpec {

  describe("A Page Parser") {

    it("should identify 'Times-Bold' as bold fond") { assert(PageParser.isBoldFont("Times-Bold")) }

    it("should identify 'Times-Roman' as non-bold fond") { assert(!PageParser.isBoldFont("Times-Roman")) }

    it("should identify empty string as non-bold fond") { assert(!PageParser.isBoldFont("")) }

    it("should identify null string object as non-bold fond") { assert(!PageParser.isBoldFont(null)) }

  }

}

