package parser

import java.text.DecimalFormat

object Rounder {

  def roundToTens(original: Float): Float = {
    val formatter = new DecimalFormat("0.0'0'")
    formatter.format(original).toFloat
  }

}
