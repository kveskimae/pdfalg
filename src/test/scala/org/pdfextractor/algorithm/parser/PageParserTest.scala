package org.pdfextractor.algorithm.parser

import org.scalatest.FunSpec

class PageParserTest extends FunSpec {

  describe("A Page Parser") {

    it("should identify 'Times-Bold' as bold fond") { assert(isBoldFont("Times-Bold")) }

    it("should identify 'Times-Roman' as non-bold fond") { assert(!isBoldFont("Times-Roman")) }

    it("should identify empty string as non-bold fond") { assert(!isBoldFont("")) }

    it("should identify null string object as non-bold fond") { assert(!isBoldFont(null)) }

  }

}

