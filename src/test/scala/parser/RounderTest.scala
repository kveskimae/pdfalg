package parser


import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RounderTest extends FunSpec {

  describe("A Rounder") {

    it("should drop digits after tens") { assert(Rounder.roundToTens(23.343f) == 23.3f) }

  }

}
