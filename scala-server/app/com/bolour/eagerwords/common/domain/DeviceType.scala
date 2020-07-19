/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

object DeviceType {

  val MouseString = "Mouse"
  val TouchString = "Touch"

  sealed abstract class DeviceType

  object MouseDevice extends DeviceType {
    override def toString = MouseString
  }

  object TouchDevice extends DeviceType {
    override def toString = TouchString
  }

  def fromString(asString: String): DeviceType = {
    asString match {
      case MouseString => DeviceType.MouseDevice
      case TouchString => DeviceType.TouchDevice
      case _ => throw new RuntimeException(s"unrecognized device type: ${asString}")
    }
  }
}

