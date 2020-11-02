package com.bolour.eagerwords.server.service

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.bolour.auth.server.service.AuthService

class AdminServiceImpl @Inject() (kernelService: AuthService, gameService: GameService) extends AdminService {
  /**
    * TODO. Use a transaction. Transaction demarcation at the service layer not yet provided!
    */
  override def removeSignedUpUser(email: String): Future[Unit] = {
    for {
      _ <- gameService.removeAllUserGameRelatedInfo(email)
      _ <- kernelService.removeSignedUpUser(email)
    } yield ()
  }
}
