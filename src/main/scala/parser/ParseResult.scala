package parser

import scala.collection.LinearSeq
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

class ParseResult(val text: String, val phrases: LinearSeq[Phrase]) {

  def findPhrasesBelow(phrase: Phrase): LinearSeq[Phrase] = {
    var matchesFound: ListBuffer[Phrase] = scala.collection.mutable.ListBuffer.empty[Phrase]
    val minYForTokenBelow = phrase.y + phrase.height
    for (token <- phrases) {
      val diff = Math.abs(token.x - phrase.x)
      if ((phrase.pageNumber.equals(token.pageNumber)) && diff < 10 && token.y >= minYForTokenBelow) matchesFound += token
    }
    matchesFound.toList
  }

  def findPhrasesAbove(phrase: Phrase): LinearSeq[Phrase] = {
    var matchesFound: ListBuffer[Phrase] = scala.collection.mutable.ListBuffer.empty[Phrase]
    val minYForTokenAbove = phrase.y
    for (token <- phrases) {
      val diff = Math.abs(token.x - phrase.x)
      if ((phrase.pageNumber.equals(token.pageNumber)) && diff < 10 && token.y < minYForTokenAbove) matchesFound += token
    }
    matchesFound.toList
  }

  def findLeftmostPhrase(phrases: LinearSeq[Phrase]): Phrase = {
    if (phrases.isEmpty) throw new IllegalArgumentException("Parameter phrases list did not contain any phrases")
    val iterator = phrases.iterator
    var closest = iterator.next
    while ( {
      iterator.hasNext
    }) {
      val compare = iterator.next
      if (compare.x < closest.x) closest = compare
    }
    closest
  }

  def findRightmostPhrase(phrases: LinearSeq[Phrase]): Phrase = {
    if (phrases.isEmpty) throw new IllegalArgumentException("Parameter phrases list did not contain any phrases")
    val iterator = phrases.iterator
    var closest = iterator.next
    while ( {
      iterator.hasNext
    }) {
      val compare = iterator.next
      if (compare.x > closest.x) closest = compare
    }
    closest
  }

  def findUpmostPhrase(phrases: LinearSeq[Phrase]): Phrase = {
    if (phrases.isEmpty) throw new IllegalArgumentException("Parameter phrases list did not contain any phrases")
    val iterator = phrases.iterator
    var closest = iterator.next
    while ( {
      iterator.hasNext
    }) {
      val compare = iterator.next
      if (compare.y < closest.y) closest = compare
    }
    closest
  }

  // TODO

  def findPhrasesOnLine(phrase: Phrase): LinearSeq[Phrase] = {
    var matchesFound: ListBuffer[Phrase] = scala.collection.mutable.ListBuffer.empty[Phrase]
    for (token <- phrases) {
      if ((phrase.pageNumber != token.pageNumber) || token.equals(phrase)) {

      } else {
        val diff = Math.abs(token.y - phrase.y)
        if (diff < 10) matchesFound += token
      }
    }
    matchesFound.toList
  }

  def findTokensOnRight(phrase: Phrase): LinearSeq[Phrase] = {
    val tokensOnLine = findPhrasesOnLine(phrase)
    val ret = scala.collection.mutable.ListBuffer.empty[Phrase]
    val iterator = tokensOnLine.iterator
    while ( {
      iterator.hasNext
    }) {
      val tokenOnLine = iterator.next
      if (tokenOnLine.x > phrase.x) ret += tokenOnLine
    }
    ret.toList
  }

  def findTokensOnLeft(phrase: Phrase): LinearSeq[Phrase] = {
    val tokensOnLine = findPhrasesOnLine(phrase)
    val ret = scala.collection.mutable.ListBuffer.empty[Phrase]
    val iterator = tokensOnLine.iterator
    while ( {
      iterator.hasNext
    }) {
      val tokenOnLine = iterator.next
      if (tokenOnLine.x < phrase.x) ret += tokenOnLine
    }
    ret.toList
  }

  def findClosestPhraseBelow(phrase: Phrase): Phrase = {
    if (phrase == null) throw new IllegalArgumentException("Parameter phrase is null")
    val phrasesBelow = findPhrasesBelow(phrase)
    if (phrasesBelow.isEmpty) return null
    val iterator = phrasesBelow.iterator
    var closest = iterator.next
    while ( {
      iterator.hasNext
    }) {
      val token = iterator.next
      if (token.y < closest.y) closest = token
    }
    closest
  }

  def findClosestPhraseAbove(phrase: Phrase): Phrase = {
    if (phrase == null) throw new IllegalArgumentException("Parameter phrase is null")
    val phrasesAbove = findPhrasesAbove(phrase)
    if (phrasesAbove.isEmpty) return null
    val iterator = phrasesAbove.iterator
    var closest = iterator.next
    while ( {
      iterator.hasNext
    }) {
      val token = iterator.next
      if (token.y > closest.y) closest = token
    }
    closest
  }

  def findClosestPhraseOnRight(phrase: Phrase): Phrase = {
    val tokensOnRight = findTokensOnRight(phrase)
    if (!tokensOnRight.isEmpty) {
      val closestToken = findLeftmostPhrase(tokensOnRight)
      return closestToken
    }
    null
  }

  def findClosestPhraseOnLeft(phrase: Phrase): Phrase = {
    val tokensOnLeft = findTokensOnLeft(phrase)
    if (!tokensOnLeft.isEmpty) {
      val closestToken = findRightmostPhrase(tokensOnLeft)
      return closestToken
    }
    null
  }

  def findClosestPhrasesBelowOrRight(phrase: Phrase): LinearSeq[Phrase] = {
    var matchesFound: ListBuffer[Phrase] = scala.collection.mutable.ListBuffer.empty[Phrase]
    val minYForTokenBelow = phrase.y + 1
    for (token <- phrases) {
      val diff = token.x - phrase.x
      if ((phrase.pageNumber.equals(token.pageNumber)) && diff > 10 && token.y >= minYForTokenBelow) matchesFound += token
    }
    val ret = scala.collection.mutable.ListBuffer.empty[Phrase]
    if (!matchesFound.isEmpty) {
      val matchesFoundAsList: List[Phrase] = matchesFound.toList
      val leftmost: Phrase = findLeftmostPhrase(matchesFoundAsList)
      val upmost: Phrase = findUpmostPhrase(matchesFoundAsList)
      if (leftmost != null) ret += leftmost
      if ((upmost ne upmost) && (upmost ne leftmost)) ret += upmost
    }
    ret.toList
  }

}
