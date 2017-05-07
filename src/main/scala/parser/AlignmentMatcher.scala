package parser

import org.apache.pdfbox.text.TextPosition

class AlignmentMatcher(var lastYCoordinate: Float,
                       var maximumXForNextCharacter: Float,
                       var maximumXForNextCharacterWithSpaceBetween: Float) {

  def isVerticalPositionMatchesPrevious(text: TextPosition): Boolean = {
    val yCoordinate: Float = Rounder.roundToTens(text.getYDirAdj)
    yCoordinate.equals(lastYCoordinate)
  }

  def isHorizontalPositionContinuesPrevious(text: TextPosition): Boolean = {
    if (text.getXDirAdj.compareTo(maximumXForNextCharacter)<= 0) true
    else if (isSpace(text)) return true
    false
  }

  def isSpace(text: TextPosition): Boolean = {
    text.getXDirAdj.compareTo(maximumXForNextCharacter) >= 0 &&
      text.getXDirAdj.compareTo(maximumXForNextCharacterWithSpaceBetween) <= 0
  }

  def setLastVerticalPosition(text: TextPosition): Unit = {
    val yCoordinate = Rounder.roundToTens(text.getYDirAdj)
    this.lastYCoordinate = yCoordinate
  }

  def setMaximumExpectedXCoordinateForNextChacater(maximumXForNextCharacter: Float): Unit = {
    this.maximumXForNextCharacter = maximumXForNextCharacter
  }

  def setMaximumXForNextCharacterWithSpaceBetween(maximumXForNextCharacterWithSpaceBetween: Float): Unit = {
    this.maximumXForNextCharacterWithSpaceBetween = maximumXForNextCharacterWithSpaceBetween
  }

}
