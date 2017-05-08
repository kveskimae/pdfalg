package exception

class AppServerFaultException(message: String, t: Throwable) extends AppAbstractException(message, t) {

  def this(message: String) {
    this(message, null)
  }

}
