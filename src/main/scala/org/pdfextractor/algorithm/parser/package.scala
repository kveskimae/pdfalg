package org.pdfextractor.algorithm

import java.io.{IOException, InputStream}
import java.text.DecimalFormat

import exception.{AppBadInputException, AppServerFaultException}
import org.apache.commons.lang3.StringUtils
import org.apache.pdfbox.pdmodel.graphics.state.PDTextState
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}
import posorder.GridConstants

import scala.collection.LinearSeq
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

package object parser {

  // Default space tolerance: 0.5, average character tolerance: 0.3
  class PageParser() extends PDFTextStripper() {

    private var nextCharacterStartsNewWord = true
    private val alignmentMatcher = new AlignmentMatcher(0f, 0f, 0f)
    private val phrases = scala.collection.mutable.ListBuffer.empty[Phrase]
    var matchesFound: ListBuffer[Phrase] = scala.collection.mutable.ListBuffer.empty[Phrase]
    private val builder = new StringBuilder
    private var x = 0f
    private var y = 0f
    private var height = 0f
    private var width = 0f
    private var bold = false

    setSortByPosition(true) // called inside constructor

    @throws[IOException]
    override protected def endPage(page: PDPage): Unit = { // And add the last phrase too
      addToPhrases()
      super.endPage(page)
    }

    override protected def processTextPosition(text: TextPosition): Unit = {
      val stringBuilder: StringBuilder = new StringBuilder
      stringBuilder.append(text.getUnicode)
      var textCharacter = stringBuilder.toString
      textCharacter = textCharacter.replaceAll("\u00A0", " ") // non-breaking spaces

      textCharacter = textCharacter.replaceAll("\u2212", "-") // minus signs

      textCharacter = textCharacter.replaceAll("\u00AD", "-") // soft hyphen

      textCharacter = textCharacter.replaceAll("\u00b0", "o") // degree symbol

      val isVerticallyAligned = alignmentMatcher.isVerticalPositionMatchesPrevious(text)
      val isHorizontallyContinued = alignmentMatcher.isHorizontalPositionContinuesPrevious(text)
      alignmentMatcher.setLastVerticalPosition(text)
      if (!isVerticallyAligned || !isHorizontallyContinued) {
        setNextCharacterToStartNewWord()
      } else if (isVerticallyAligned && alignmentMatcher.isSpace(text)) {
        nextCharacterStartsNewWord = false
        builder.append(' ')
        width = 2 * roundToTens(text.getXDirAdj) + text.getWidth - x
      }
      if (nextCharacterStartsNewWord) {
        nextCharacterStartsNewWord = false
        startNewWord(text, textCharacter)
      }
      else if (isVerticallyAligned) {
        nextCharacterStartsNewWord = false
        builder.append(textCharacter)
        width = roundToTens(text.getXDirAdj) + text.getWidth - x
      }
      val textState: PDTextState = getGraphicsState.getTextState
      val fontSize = textState.getFontSize
      val charSpacing = textState.getCharacterSpacing
      val horizontalScaling = textState.getHorizontalScaling / 100.0F
      val delta = text.getWidthDirAdj + fontSize + charSpacing
      var maximumXForNextCharacterWithSpaceBetween = text.getXDirAdj + 2 * delta
      maximumXForNextCharacterWithSpaceBetween *= horizontalScaling
      alignmentMatcher.setMaximumExpectedXCoordinateForNextChacater(text.getXDirAdj + text.getWidthDirAdj + text.getWidthOfSpace) // Space width is just in case

      alignmentMatcher.setMaximumXForNextCharacterWithSpaceBetween(maximumXForNextCharacterWithSpaceBetween)
      super.processTextPosition(text)
    }

    private def setNextCharacterToStartNewWord() = {
      nextCharacterStartsNewWord = true
      if (x < 0) x = 0
      else if (x > GridConstants.A4_WIDTH_PX - 1) x = GridConstants.A4_WIDTH_PX - 1
      if (y < 0) y = 0
      else if (y > GridConstants.A4_HEIGHT_PX - 1) y = GridConstants.A4_HEIGHT_PX - 1
      addToPhrases()
    }

    private def addToPhrases() = {
      val builderResult: String = builder.toString
      if (!builderResult.isEmpty) {
        val phrase: Phrase =
          new Phrase(
            Math.round(x),
            Math.round(y),
            getCurrentPageNo,
            Math.round(height),
            Math.round(width),
            builderResult,
            bold)

        phrases += phrase
      }
    }

    private def startNewWord(text: TextPosition, tChar: String) = {
      bold = isBoldFont(text.getFont.getName)
      builder.setLength(0)
      x = roundToTens(text.getXDirAdj)
      y = roundToTens(text.getYDirAdj)
      height = text.getFontSizeInPt
      width = text.getWidth
      builder.append(tChar)
    }

    def getPhrases: LinearSeq[Phrase] = phrases.toList


  }


  object PDFFileParser {

    def parse(pdfContentStream: InputStream): ParseResult = {

      val document: PDDocument = try {
        PDDocument.load(pdfContentStream)
      } catch {
        case e: IOException =>
          throw new AppBadInputException("PDF file is corrupt or not supported")
      }

      val ret: ParseResult = try {
        val processor: PageParser = new PageParser()
        val text: String = processor.getText(document)
        val phrases: LinearSeq[Phrase]  = processor.getPhrases
        new ParseResult(text, phrases)
      } catch {case e: IOException => throw new AppServerFaultException("Parsing PDF file failed") }

      try {
        document.close
      } catch {
        case ignored: IOException =>
      }

      ret
    }

  }

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

    def findClosestPhraseBelow(phrase: Phrase): Option[Phrase] = {
      Option(phrase).orElse(throw new NullPointerException)
      val phrasesBelow = findPhrasesBelow(phrase)
      if (phrasesBelow.isEmpty) {
        None
      } else {
        val iterator = phrasesBelow.iterator
        var closest = iterator.next
        while ( {
          iterator.hasNext
        }) {
          val token = iterator.next
          if (token.y < closest.y) closest = token
        }
        Some(closest)
      }
    }

    def findClosestPhraseAbove(phrase: Phrase): Option[Phrase] = {
      Option(phrase).orElse(throw new NullPointerException)
      val phrasesAbove = findPhrasesAbove(phrase)
      if (phrasesAbove.isEmpty) {
        None
      } else {
        val iterator = phrasesAbove.iterator
        var closest = iterator.next
        while ( {
          iterator.hasNext
        }) {
          val token = iterator.next
          if (token.y > closest.y) closest = token
        }
        Some(closest)
      }
    }

    def findClosestPhraseOnRight(phrase: Phrase): Option[Phrase] = {
      val tokensOnRight = findTokensOnRight(phrase)
      if (!tokensOnRight.isEmpty) {
        val closestToken = findLeftmostPhrase(tokensOnRight)
        Some(closestToken)
      } else {
        None
      }
    }

    def findClosestPhraseOnLeft(phrase: Phrase): Option[Phrase] = {
      val tokensOnLeft = findTokensOnLeft(phrase)
      if (!tokensOnLeft.isEmpty) {
        val closestToken = findRightmostPhrase(tokensOnLeft)
        Option(closestToken)
      } else {
       Option.empty
      }
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
        ret += leftmost
        val upmost: Phrase = findUpmostPhrase(matchesFoundAsList)
        if (upmost != leftmost) ret += upmost
      }
      ret.toList
    }

  }

  case class Phrase(val x: Int, val y: Int, val pageNumber: Int, val height: Int, val width: Int, val text: String, val bold: Boolean);

  class AlignmentMatcher(var lastYCoordinate: Float,
                         var maximumXForNextCharacter: Float,
                         var maximumXForNextCharacterWithSpaceBetween: Float) {

    def isVerticalPositionMatchesPrevious(text: TextPosition): Boolean = {
      val yCoordinate: Float = roundToTens(text.getYDirAdj)
      yCoordinate.equals(lastYCoordinate)
    }

    def isHorizontalPositionContinuesPrevious(text: TextPosition): Boolean = {
      if (text.getXDirAdj.compareTo(maximumXForNextCharacter) <= 0) {
        true
      }
      else if (isSpace(text)) {
        true
      }
      else {
        false
      }
    }

    def isSpace(text: TextPosition): Boolean = {
      text.getXDirAdj.compareTo(maximumXForNextCharacter) >= 0 &&
        text.getXDirAdj.compareTo(maximumXForNextCharacterWithSpaceBetween) <= 0
    }

    def setLastVerticalPosition(text: TextPosition): Unit = {
      val yCoordinate = roundToTens(text.getYDirAdj)
      this.lastYCoordinate = yCoordinate
    }

    def setMaximumExpectedXCoordinateForNextChacater(maximumXForNextCharacter: Float): Unit = {
      this.maximumXForNextCharacter = maximumXForNextCharacter
    }

    def setMaximumXForNextCharacterWithSpaceBetween(maximumXForNextCharacterWithSpaceBetween: Float): Unit = {
      this.maximumXForNextCharacterWithSpaceBetween = maximumXForNextCharacterWithSpaceBetween
    }

  }

  def roundToTens(original: Float): Float = {
    val formatter = new DecimalFormat("0.0'0'")
    formatter.format(original).toFloat
  }

  private val Bold = "(?i)BOLD".r // Text style

  def isBoldFont(fontName: String): Boolean = {
    !StringUtils.isBlank(fontName) && Bold.findFirstIn(fontName).nonEmpty
  }

}