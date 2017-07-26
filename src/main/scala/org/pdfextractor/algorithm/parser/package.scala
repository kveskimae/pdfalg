package org.pdfextractor.algorithm

import java.io.{IOException, InputStream}
import java.text.DecimalFormat

import exception.{AppBadInputException, AppServerFaultException}
import org.apache.commons.lang3.StringUtils
import org.apache.pdfbox.pdmodel.graphics.state.PDTextState
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}

import scala.collection.{LinearSeq, Traversable, mutable}
import scala.collection.mutable.ListBuffer
import org.pdfextractor.algorithm.candidate.posorder._

package object parser {

  // Default space tolerance: 0.5, average character tolerance: 0.3
  class PageParser(private var nextCharacterStartsNewWord: Boolean = true,
                   private val alignmentMatcher: AlignmentMatcher =
                     new AlignmentMatcher(0f, 0f, 0f),
                   private val phrases: mutable.ListBuffer[Phrase] =
                     mutable.ListBuffer.empty,
                   private var matchesFound: ListBuffer[Phrase] =
                     mutable.ListBuffer.empty,
                   private val builder: StringBuilder = new StringBuilder,
                   private var x: Float = 0f,
                   private var y: Float = 0f,
                   private var height: Float = 0f,
                   private var width: Float = 0f,
                   private var bold: Boolean = false)
      extends PDFTextStripper {

    setSortByPosition(true)

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

      val isVerticallyAligned =
        alignmentMatcher.isVerticalPositionMatchesPrevious(text)
      val isHorizontallyContinued =
        alignmentMatcher.isHorizontalPositionContinuesPrevious(text)
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
      } else if (isVerticallyAligned) {
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
      alignmentMatcher.maximumXForNextCharacter = text.getXDirAdj + text.getWidthDirAdj + text.getWidthOfSpace // Space width is just in case

      alignmentMatcher.maximumXForNextCharacterWithSpaceBetween =
        maximumXForNextCharacterWithSpaceBetween
      super.processTextPosition(text)
    }

    private def setNextCharacterToStartNewWord() = {
      nextCharacterStartsNewWord = true
      if (x < 0) x = 0
      else if (x > A4WidthPx - 1) x = A4WidthPx - 1
      if (y < 0) y = 0
      else if (y > A4HeightPx - 1) y = A4HeightPx - 1
      addToPhrases()
    }

    private def addToPhrases() = {
      val builderResult: String = builder.toString
      if (!builderResult.isEmpty) {
        val phrase: Phrase =
          new Phrase(Math.round(x),
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
        val phrases: LinearSeq[Phrase] = processor.getPhrases
        new ParseResult(text, phrases)
      } catch {
        case e: IOException =>
          throw new AppServerFaultException("Parsing PDF file failed")
      }

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
      phrases.filter(
        token =>
          phrase.pageNumber.equals(token.pageNumber) &&
            Math.abs(token.x - phrase.x) < 10 &&
            token.y >= phrase.y + phrase.height)
    }

    def findPhrasesAbove(phrase: Phrase): LinearSeq[Phrase] = {
      phrases.filter(
        token =>
          phrase.pageNumber == token.pageNumber &&
            Math.abs(token.x - phrase.x) < 10 &&
            token.y < phrase.y)
    }

    // TODO

    def findPhrasesOnLine(phrase: Phrase): LinearSeq[Phrase] = {
      phrases.filter(
        token =>
          token.pageNumber == phrase.pageNumber &&
            token != phrase &&
            Math.abs(token.y - phrase.y) < 10)
    }

    def findTokensOnRight(phrase: Phrase): LinearSeq[Phrase] = {
      findPhrasesOnLine(phrase).filter(_.x > phrase.x)
    }

    def findTokensOnLeft(phrase: Phrase): LinearSeq[Phrase] = {
      findPhrasesOnLine(phrase).filter(_.x < phrase.x)
    }

    def findClosestPhraseBelow(phrase: Phrase): Option[Phrase] = {
      Option(phrase).orElse(throw new NullPointerException)
      val phrasesBelow = findPhrasesBelow(phrase)
      if (phrasesBelow.isEmpty) {
        None
      } else {
        val iterator = phrasesBelow.iterator
        var closest = iterator.next
        while ({
          iterator.hasNext
        }) {
          val token = iterator.next
          if (token.y < closest.y) closest = token
        }
        Some(closest)
      }
    }

    def findClosestPhraseAbove(phrase: Phrase): Option[Phrase] = {
      require(Option(phrase).isDefined)

      val above = findPhrasesAbove(phrase)

      if (above.nonEmpty) {
        Some(
          above.reduceLeft((p1: Phrase, p2: Phrase) => {
            if (p1.y > p2.y) p1
            else p2
          })
        )
      } else {
        None
      }
    }

    def findClosestPhraseOnRight(phrase: Phrase): Option[Phrase] = {
      val tokensOnRight = findTokensOnRight(phrase)
      if (!tokensOnRight.isEmpty) {
        val closestToken = leftmost(tokensOnRight)
        Some(closestToken)
      } else {
        None
      }
    }

    def findClosestPhraseOnLeft(phrase: Phrase): Option[Phrase] = {
      val tokensOnLeft = findTokensOnLeft(phrase)
      if (!tokensOnLeft.isEmpty) {
        val closestToken = rightmost(tokensOnLeft)
        Option(closestToken)
      } else {
        Option.empty
      }
    }

    def findClosestPhrasesBelowOrRight(phrase: Phrase): LinearSeq[Phrase] = {
      val matchesFoundAsList = phrases
        .filter(token => phrase.pageNumber.equals(token.pageNumber))
        .filter(token => token.x - phrase.x > 10)
        .filter(token => token.y >= phrase.y + 1)

      if (!matchesFoundAsList.isEmpty) {
        val leftmostPhrase: Phrase = leftmost(matchesFoundAsList)
        val uppermostPhrase: Phrase = uppermost(matchesFoundAsList)
        if (uppermostPhrase != leftmostPhrase)
          LinearSeq(leftmostPhrase, uppermostPhrase)
        else LinearSeq(leftmostPhrase)
      } else {
        LinearSeq.empty
      }
    }

  }

  case class Phrase(val x: Int,
                    val y: Int,
                    val pageNumber: Int,
                    val height: Int,
                    val width: Int,
                    val text: String,
                    val bold: Boolean);

  class AlignmentMatcher(var lastYCoordinate: Float,
                         var maximumXForNextCharacter: Float,
                         var maximumXForNextCharacterWithSpaceBetween: Float) {

    def isVerticalPositionMatchesPrevious(text: TextPosition): Boolean = {
      val yCoordinate: Float = roundToTens(text.getYDirAdj)
      yCoordinate.equals(lastYCoordinate)
    }

    def isHorizontalPositionContinuesPrevious(text: TextPosition): Boolean = {
      text.getXDirAdj.compareTo(maximumXForNextCharacter) <= 0 || isSpace(text)
    }

    def isSpace(text: TextPosition): Boolean = {
      text.getXDirAdj.compareTo(maximumXForNextCharacter) >= 0 &&
      text.getXDirAdj.compareTo(maximumXForNextCharacterWithSpaceBetween) <= 0
    }

    def setLastVerticalPosition(text: TextPosition): Unit = {
      val yCoordinate = roundToTens(text.getYDirAdj)
      this.lastYCoordinate = yCoordinate
    }

  }

  def roundToTens(original: Float): Float = {
    val formatter = new DecimalFormat("0.0'0'")
    formatter.format(original).toFloat
  }

  val Bold = "(?i)BOLD".r

  def isBoldFont(fontName: String): Boolean = {
    !StringUtils.isBlank(fontName) && Bold.findFirstIn(fontName).nonEmpty
  }

  def leftmost(phrases: Traversable[Phrase]): Phrase = {
    findWithReduce(phrases, (p1, p2) => if (p1.x < p2.x) p1 else p2)
  }

  def rightmost(phrases: Traversable[Phrase]): Phrase = {
    findWithReduce(phrases, (p1, p2) => if (p1.x > p2.x) p1 else p2)
  }

  def uppermost(phrases: Traversable[Phrase]): Phrase = {
    findWithReduce(phrases, (p1, p2) => if (p1.y < p2.y) p1 else p2)
  }

  private def findWithReduce(phrases: Traversable[Phrase],
                             fn: (Phrase, Phrase) => Phrase): Phrase = {
    require(phrases.nonEmpty)

    phrases.reduce(fn)
  }

}
