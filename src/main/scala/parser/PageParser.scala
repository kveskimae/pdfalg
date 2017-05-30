package parser

import java.io.IOException
import java.util.regex.Pattern

import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.pdmodel.graphics.state.PDTextState
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}
import posorder.GridConstants
import regex.RegexUtils

import scala.collection.LinearSeq
import scala.collection.mutable.ListBuffer

object PageParser {

  private val PATTERN_BOLD = Pattern.compile("BOLD", Pattern.CASE_INSENSITIVE) // Text style

  def isBoldFont(fontName: String): Boolean = {
    if (fontName == null) return false
    RegexUtils.patternExistsInText(fontName, PATTERN_BOLD)
  }

}

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
      width = 2 * Rounder.roundToTens(text.getXDirAdj) + text.getWidth - x
    }
    if (nextCharacterStartsNewWord) {
      nextCharacterStartsNewWord = false
      startNewWord(text, textCharacter)
    }
    else if (isVerticallyAligned) {
      nextCharacterStartsNewWord = false
      builder.append(textCharacter)
      width = Rounder.roundToTens(text.getXDirAdj) + text.getWidth - x
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
    if (!builderResult.isEmpty)
      {
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
    bold = PageParser.isBoldFont(text.getFont.getName)
    builder.setLength(0)
    x = Rounder.roundToTens(text.getXDirAdj)
    y = Rounder.roundToTens(text.getYDirAdj)
    height = text.getFontSizeInPt
    width = text.getWidth
    builder.append(tChar)
  }

  def getPhrases: LinearSeq[Phrase] = phrases.toList


}
