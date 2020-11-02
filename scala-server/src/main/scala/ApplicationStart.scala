import javax.inject._
import scala.concurrent.duration._
import scala.concurrent.{Future, _}
import play.api.inject.ApplicationLifecycle

import com.bolour.auth.server.service.AuthService
import com.bolour.eagerwords.server.service.GameService

@Singleton
class ApplicationStart @Inject()(lifecycle: ApplicationLifecycle, kernelService: AuthService, gameService: GameService) {

  lifecycle.addStopHook { () =>
    Future.successful(())
  }

  // TODO. URGENT. Proper database migration.
  Await.result(kernelService.migrate(), 30 seconds)
  Await.result(gameService.migrate(), 30 seconds)

}
