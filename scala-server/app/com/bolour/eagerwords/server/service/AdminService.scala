package com.bolour.eagerwords.server.service

import scala.concurrent.Future

trait AdminService {
  def removeSignedUpUser(email: String): Future[Unit]
}
