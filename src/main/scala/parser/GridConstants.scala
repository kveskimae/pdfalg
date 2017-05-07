package parser

object GridConstants {

  /**
    * PDFBox library has this number for A4 page size width in pixels
    */
  val A4_WIDTH_PX = 596

  /**
    * PDFBox library has this number for A4 page size height in pixels
    */
  val A4_HEIGHT_PX = 838

  /**
    * No grid cell is split into smaller cells if it contains less than or
    * equal to this number of data points
    */
  val MIN_NUMBER_OF_DATA_POINTS_IN_CELL = 2

  /**
    * Minimum side length for a cell, so that cells do not become too small
    */
  val MIN_CELL_LENGTH_PX = (A4_WIDTH_PX - 4) / 4

}
