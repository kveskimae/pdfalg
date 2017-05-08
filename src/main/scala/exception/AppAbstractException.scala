package exception

class AppAbstractException(message: String, t: Throwable) extends RuntimeException(message, t) {

  def this(message: String) {
    this(message, null)
  }

}
