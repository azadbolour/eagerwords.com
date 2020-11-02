/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.util.message

/**
  * Data transfer objects for basic application errors.
  */
sealed abstract class BaseErrorDto

case class SystemOverloadedErrorDto(tag: String, message: String) extends BaseErrorDto

case class InternalErrorDto(tag: String, message: String) extends BaseErrorDto
