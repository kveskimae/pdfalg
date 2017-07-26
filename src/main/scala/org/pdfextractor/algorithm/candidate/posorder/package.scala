package org.pdfextractor.algorithm.candidate

package object posorder {

  /**
    * PDFBox library has this number for A4 page size width in pixels
    */
  val A4WidthPx = 596

  /**
    * PDFBox library has this number for A4 page size height in pixels
    */
  val A4HeightPx = 838

  /**
    * No grid cell is split into smaller cells if it contains less than or
    * equal to this number of data points
    */
  val CellMinPoints = 2

  /**
    * Minimum side length for a cell, so that cells do not become too small
    */
  val CellMinLengthPx = (A4WidthPx - 4) / 4

  sealed trait CutDirection

  /**
    *
    * |-----|
    * |  x  |
    * |  x  |
    * |  x  |
    * |  x  |
    * |  x  |
    * |_____|
    *
    * Cutting along the vertical line
    *
    */
  case object Vertical extends CutDirection

  /**
    *
    * |-----|
    * |     |
    * |     |
    * |xxxxx|
    * |     |
    * |     |
    * |_____|
    *
    * Cutting along the horizontal line
    *
    */
  case object Horizontal extends CutDirection

}
