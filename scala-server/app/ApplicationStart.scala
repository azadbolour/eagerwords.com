import com.bolour.app.kernel.server.service.KernelService
import com.bolour.eagerwords.server.service.GameService

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.Future
import javax.inject._
import play.api.inject.ApplicationLifecycle

@Singleton
class ApplicationStart @Inject()(lifecycle: ApplicationLifecycle, kernelService: KernelService, gameService: GameService) {

  lifecycle.addStopHook { () =>
    Future.successful(())
  }

  // TODO. URGENT. Proper database migration.
  Await.result(kernelService.migrate(), 30 seconds)
  Await.result(gameService.migrate(), 30 seconds)

}