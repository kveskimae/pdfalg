package org.pdfextractor.algorithm.exception

class AppBadInputException(message: String, t: Option[Throwable] = None)
  extends AppAbstractException(message, t) {
  def this(message: String) = this(message, None)

  def this(message: String, t: Throwable) = this(message, Some(t))
}
