package regex

import org.scalatest._

import scala.collection.immutable.List

class RegexUtilsTest  extends FlatSpec with Matchers {

  "A RegexUtils" should "find a double value" in {
    val doubleValues: List[String] = RegexUtils.searchForDoubleValues("Totale 1,191.62 €")

    assert(1 == doubleValues.size)
    assert(doubleValues.contains("1,191.62"))
  }

  "A RegexUtils" should "not find a number in a comma" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn(",").isEmpty)
  }

  "A RegexUtils" should "not find a number in a dot" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn(".").isEmpty)
  }

  "A RegexUtils" should "regard a single digit as a number" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn("1").nonEmpty)
  }

  "A RegexUtils" should "regard several digits as a number" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn("123").nonEmpty)
  }

  "A RegexUtils" should "regard several digits with a dot as a number" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn("123.456").nonEmpty)
  }

  "A RegexUtils" should "regard several digits with several dots as a number" in {
    assert(CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findFirstIn("123.456.789").nonEmpty)
  }

  "A RegexUtils" should "not find a double value from a date formatted with slashes" in {
    val found: List[String] = RegexUtils.searchForDoubleValues("19/05/2016")

    assert(found.size == 0)
  }

}