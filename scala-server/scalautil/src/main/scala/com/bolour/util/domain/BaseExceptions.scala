package com.bolour.util.domain

object BaseExceptions {

  sealed abstract class BaseException(cause: Throwable = null) extends Exception(cause) {
    def causeMessage: String = if (cause == null) "" else s" - caused by: ${cause.getMessage}"
  }

  case class SystemOverloadedException() extends BaseException {
    override def getMessage: String = "system overloaded"
  }

  case class InternalAppException(message: String, cause: Throwable) extends BaseException(cause) {
    override def getMessage: String = s"${message}${causeMessage}"
  }

}
