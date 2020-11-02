/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.server.domain

import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

import com.bolour.eagerwords.server.domain.GameExceptions.InvalidCrosswordsException

class GameExceptionSpec extends FlatSpec with Matchers {
  val logger = LoggerFactory.getLogger(this.getClass)

  "invalid crosswords" should "produce reasonable message" in {
    val invalidWords = List("prindle", "gradler")
    val exception = InvalidCrosswordsException("en", invalidWords)
    val message = exception.getMessage
    logger.info(message)
  }

}
