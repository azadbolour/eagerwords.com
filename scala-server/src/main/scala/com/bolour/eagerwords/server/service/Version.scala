package com.bolour.eagerwords.server.service

/**
  * Version of the server.
  *
  * For now it serves as both the API version for clients,
  * and the persistence version for specific representations
  * of persisted objects.
  */
object Version {
  val version: Int = 1
}
