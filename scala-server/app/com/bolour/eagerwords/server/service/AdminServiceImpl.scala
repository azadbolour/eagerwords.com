package com.bolour.eagerwords.server.service
import com.bolour.app.kernel.server.service.KernelService
import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

class AdminServiceImpl @Inject() (kernelService: KernelService, gameService: GameService) extends AdminService {
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
