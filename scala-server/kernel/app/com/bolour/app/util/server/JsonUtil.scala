/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.util.server

import spray.json.{JsString, JsValue}

object JsonUtil {

  def removeQuotes(s: String): String = {
    s.replaceAll("\"", "")
  }

  /**
    * There must be a simpler way to convert a json string that has been
    * quoted by spray, to a simple string value, or to prevent spray to add
    * the quotes in the first place, but I have missed it so far!
    *
    * @param json A JsString presented as a JsValue.
    * @return The enclosed string without quote.
    */
  def unwrapQuotedJsonString(json: JsValue): String = {
    val value = json.asInstanceOf[JsString]
    removeQuotes(value.value)
  }

}
