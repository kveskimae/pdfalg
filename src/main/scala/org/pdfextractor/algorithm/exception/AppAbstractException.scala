package org.pdfextractor.algorithm.exception

class AppAbstractException(message: String, t: Throwable) extends RuntimeException(message, t) {
  def this(message: String, t: Option[Throwable]) =  this(message, t.orNull)
}
