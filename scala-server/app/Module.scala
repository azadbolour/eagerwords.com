/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
import com.google.inject.{AbstractModule, Provides}
import java.time.Clock

import com.bolour.app.kernel.server.service.{KernelService, KernelServiceImpl, SecretService, SecretServiceConfigImpl}
import com.bolour.eagerwords.server.service.{GameService, GameServiceImpl}
import com.typesafe.config.ConfigFactory
import play.api.{Configuration, Environment}

// import services.{ApplicationTimer, AtomicCounter, Counter}

/**
  * This class is a Guice module that tells Guice how to bind several
  * different types. This Guice module is created when the Play
  * application starts.
  *
  * Play will automatically use any class called `Module` that is in
  * the root package. You can create modules in other locations by
  * adding `play.modules.enabled` settings to the `application.conf`
  * configuration file.
  *
  * The environment and configuration are automatically provided by
  * the play framework. Configuration is a wrapper on Conf from application.conf.
  * TODO. Study environment functions.
  */
class Module(environment: Environment, configuration: Configuration) extends AbstractModule {

  val conf = configuration.underlying

  override def configure() = {
    // Use the system clock as the default implementation of Clock
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)
    val theSecretService = new SecretServiceConfigImpl(conf)
    bind(classOf[SecretService]).toInstance(theSecretService)
    /*
     * TODO. URGENT. This can fail on initializing the underlying persister.
     * Fail fast by shutting down in case of exceptions in Module configuration in general.
     * Is that done automatically by Play?
     */
    val appService = new KernelServiceImpl(conf, theSecretService, None)
    bind(classOf[KernelService]).toInstance(appService)
    bind(classOf[GameService]).toInstance(new GameServiceImpl(conf, None, appService))
    bind(classOf[ApplicationStart]).asEagerSingleton()
  }

//  @Provides
//  def provideGameService: GameService = {
//    val service = new GameServiceImpl(ConfigFactory.load())
//    service.migrate() // TODO. This is a hack to get going. How to do play-idiomatic migration. See play-slick.
//    service
//  }

}
