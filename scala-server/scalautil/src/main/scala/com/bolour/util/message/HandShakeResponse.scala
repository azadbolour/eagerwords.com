package com.bolour.util.message

/**
  * API response for the initial handshake.
  *
  * @param serverType - Scala, ...
  * @param apiVersion
  */
case class HandShakeResponse(
  serverType: String,
  apiVersion: String
)
