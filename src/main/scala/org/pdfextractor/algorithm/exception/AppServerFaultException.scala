package org.pdfextractor.algorithm.exception

class AppServerFaultException(message: String, t: Option[Throwable])
    extends AppAbstractException(message, t) {
  def this(message: String) = this(message, None)
  def this(message: String, t: Throwable) = this(message, Some(t))
}
