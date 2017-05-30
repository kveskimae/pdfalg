package posorder

sealed trait CutDirectionType

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
case object VERTICAL extends CutDirectionType

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
case object HORIZONTAL extends CutDirectionType