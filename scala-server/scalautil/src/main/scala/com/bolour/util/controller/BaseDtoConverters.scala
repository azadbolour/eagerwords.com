package com.bolour.util.controller

import com.bolour.util.domain.BaseExceptions.{InternalAppException, SystemOverloadedException}
import com.bolour.util.message.{InternalErrorDto, SystemOverloadedErrorDto}

object BaseDtoConverters {

  def toSystemOverloadedErrorDto(ex: SystemOverloadedException) =
    SystemOverloadedErrorDto("SystemOverloadedError", ex.getMessage)

  def toInternalErrorDto(ex: InternalAppException) =
    InternalErrorDto("InternalError", ex.getMessage)
}
