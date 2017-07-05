package regex

import java.util

import org.apache.commons.lang3.StringUtils
import regex.CommonRegex.PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX

import scala.collection.mutable.ListBuffer

object RegexUtils {

  // def removeFirstOccurrence(text: String, regex: Regex): String = {
  //  regex.replaceFirstIn(text, "")
  // }

  def searchForEstonianDoubleValuesAfterText(searchString: String): util.List[Double] = {
    val ret = new util.ArrayList[Double]
    val foundDoubles: Iterator[String] = searchForDoubleValues(searchString).iterator
    for (totalAsString: String <- foundDoubles) {
      val totalAsStringReplaced = totalAsString.replaceAll(",", ".")
      val dotCount = StringUtils.countMatches(totalAsStringReplaced, ".")
      if (dotCount < 2) {
        val totalAsDouble: Double = totalAsStringReplaced.toDouble
        ret.add(totalAsDouble)
      }
    }
    ret
  }

  def searchForDoubleValues(searchString: String): List[String] = {
    var ret: ListBuffer[String] = scala.collection.mutable.ListBuffer.empty[String]
    val totalAsNumberMatcher = PATTERN_DIGITS_WITH_COMMAS_AND_DOTS_AS_REGEX.findAllIn(searchString)
    while ( {
      totalAsNumberMatcher.hasNext
    }) {
      val totalAsString = totalAsNumberMatcher.next
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

}
