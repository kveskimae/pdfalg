package regex

import java.util.regex.Pattern

import regex.CommonRegexPatterns.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS

import scala.collection.mutable.ListBuffer


object RegexUtils {

  import java.util

  import org.apache.commons.lang3.StringUtils

  def removeFirstOccurrence(text: String, pattern: Pattern): String = {
    val matcher = pattern.matcher(text)
    var `match`: String = null
    if (matcher.find) `match` = matcher.group
    else throw new IllegalArgumentException("No match of pattern " + pattern + " was found in text '" + text + "'")
    text.replaceFirst(`match`, "")
  }

  def findMFirstMatch(text: String, pattern: Pattern): String = {
    val matcher = pattern.matcher(text)
    if (matcher.find) {
      val `match` = matcher.group
      return `match`
    }
    throw new IllegalArgumentException("No match of pattern " + pattern + " was found in text '" + text + "'")
  }

  def findMatches(text: String, pattern: Pattern): ListBuffer[String] = {
    val foundMatches: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer.empty[String]
    val matcher = pattern.matcher(text)
    while ( {
      matcher.find
    }) {
      val `match` = matcher.group
      foundMatches += `match`
    }
    foundMatches
  }

  def searchForEstonianDoubleValuesAfterText(searchString: String): util.List[Double] = {
    val ret = new util.ArrayList[Double]
    val foundDoubles: Iterator[String] = searchForDoubleValues(searchString).iterator
    for (totalAsString: String <- foundDoubles) {
      val totalAsStringReplaced = totalAsString.replaceAll(",", ".")
      val dotCount = StringUtils.countMatches(totalAsStringReplaced, ".")
      if (dotCount < 2) {
        val totalAsDouble = Double.unbox(totalAsStringReplaced)
        ret.add(totalAsDouble)
      }
    }
    ret
  }

  def searchForDoubleValues(searchString: String): List[String] = {
    var ret: ListBuffer[String] = scala.collection.mutable.ListBuffer.empty[String]
    val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS.matcher(searchString)
    while ( {
      totalAsNumberMatcher.find
    }) {
      val totalAsString = totalAsNumberMatcher.group
      val endIdx = totalAsNumberMatcher.end
      val startIdx = totalAsNumberMatcher.start - 1
      var include = true
      if (endIdx < searchString.length) {
        val charAtEnd = searchString.charAt(endIdx)
        if (charAtEnd == '/' || charAtEnd == '-') include = false
      }
      if (startIdx >= 0) {
        val charAtStart = searchString.charAt(startIdx)
        if (charAtStart == '/') include = false
      }
      if (include) ret += totalAsString
    }
    ret.toList
  }

  def patternMatches(text: String, pattern: Pattern): Boolean = {
    val matcher = pattern.matcher(text)
    matcher.find
  }

  def patternExistsInText(text: String, pattern: Pattern): Boolean = {
    val matcher = pattern.matcher(text)
    matcher.find
  }

  def fixWhiteSpace(s: String): String = {
    s.replaceAll("\\s+", " ").trim
  }
}
