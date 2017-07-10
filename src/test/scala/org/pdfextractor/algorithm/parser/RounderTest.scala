package org.pdfextractor.algorithm.parser


import org.scalatest.FunSpec

class RounderTest extends FunSpec {

  describe("A Rounder") {

    it("should drop digits after tens") { assert(roundToTens(23.343f) == 23.3f) }

  }

}
