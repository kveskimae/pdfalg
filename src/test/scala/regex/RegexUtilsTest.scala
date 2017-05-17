package regex

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.List

@RunWith(classOf[JUnitRunner])
class RegexUtilsTest  extends FlatSpec with Matchers {

  "A RegexUtils" should "find a double value" in {
    val doubleValues: List[String] = RegexUtils.searchForDoubleValues("Totale 1,191.62 €")

    assert(1 == doubleValues.size)
    assert(doubleValues.contains("1,191.62"))
  }

  "A RegexUtils" should "not find a number in a comma" in {
    val commaIsFalsePositive: Boolean = RegexUtils.patternExistsInText(",", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(!commaIsFalsePositive)
  }

  "A RegexUtils" should "not find a number in a dot" in {
    val dotIsFalsePositive: Boolean = RegexUtils.patternExistsInText(".", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(!dotIsFalsePositive)
  }

  "A RegexUtils" should "regard a single digit as a number" in {
    val found: Boolean = RegexUtils.patternExistsInText("1", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(found)
  }

  "A RegexUtils" should "regard several digits as a number" in {
    val found: Boolean = RegexUtils.patternExistsInText("123", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(found)
  }

  "A RegexUtils" should "regard several digits with a dot as a number" in {
    val found: Boolean = RegexUtils.patternExistsInText("123.456", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(found)
  }

  "A RegexUtils" should "regard several digits with several dots as a number" in {
    val found: Boolean = RegexUtils.patternExistsInText("123.456.789", CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS)

    assert(found)
  }

  "A RegexUtils" should "not find a double value from a date formatted with slashes" in {
    val found: List[String] = RegexUtils.searchForDoubleValues("19/05/2016")

    assert(found.size == 0)
  }

}