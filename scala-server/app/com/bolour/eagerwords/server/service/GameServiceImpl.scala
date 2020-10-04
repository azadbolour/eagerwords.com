/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.service

import org.slf4j.LoggerFactory
import com.typesafe.config.Config

import scala.collection.immutable.Nil
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{Map => MutableMap}
import scala.util.{Failure, Success, Try}
import java.util.concurrent.ConcurrentHashMap

import javax.inject.Inject
import com.bolour.util.TimeUtil._
import com.bolour.util.CommonUtil.{ID, optionToTry}
import com.bolour.app.util.server.KernelUtil.readConfigStringList
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain._
import com.bolour.eagerwords.common.domain.PlayerType._
import com.bolour.app.kernel.server.domain.KernelExceptions.{AccessViolationException, InternalAppException, MissingUserException, NotSignedUpException, SystemOverloadedException}
import com.bolour.app.kernel.server.domain.{EmailConstants, User, Version}
import com.bolour.app.kernel.server.service.KernelService
import com.bolour.eagerwords.server.domain._
import com.bolour.eagerwords.server.domain.GameExceptions._
import com.bolour.language.domain.LanguageExceptions.{MissingDictionaryException, UnsupportedLanguageException}
import com.bolour.language.domain.WordDictionary
import com.bolour.plane.domain.Point
import com.bolour.util.{FrequencyDistribution, SymmetricCryptJava}


/**
  * Implementation of the service layer.
  *
  * In the service layer API, the user is always identified by his external
  * unique identifier. In internal code below the service layer, the user is
  * always identified by his internal (database) identifier. That way the
  * external identifier, which is dependent on our particular authentication
  * provider, would not pollute our internal classes.
  *
  * Of course, it goes without saying that to obtain the internal identifier
  * of a user given his external identifier, some lower level calls need
  * to get the external identifier as a parameter. But that is the extent
  * of the use of external identifiers in lower-level APIs.
  *
  * @param config The configuration of the game service.
  * @param configuredDbName The name of the specific database to use as configured in application.conf, if given.
  *                         Use "defaultDb" if none given.
  */
class GameServiceImpl @Inject() (config: Config, configuredDbName: Option[String], appService: KernelService) extends GameService {

  import GameService._
  import GameServiceImpl._
  import StripMatcher.findAndSetBoardBlackPoints

  val logger = LoggerFactory.getLogger(this.getClass)

  val dbConfigPrefix = confPath("db")
  val dbName = configuredDbName.getOrElse("defaultDb");
  val defaultDbPath = s"${dbConfigPrefix}.${dbName}"
  val MaxMaskedLetters = 3

  // TODO. Validate all config parameters and throw meaningful exceptions.

  // TODO. Validate service method parameters.
  // To the extent validation code is implementation-independent,
  // implement in the base trait.

  val dictionaryDirConfigPath = confPath("dictionaryDir")

  val maxActiveGames = config.getInt(maxActiveGamesConfigPath)
  val maxGameIdleMinutes = config.getInt(maxGameIdleMinutesConfigPath)
  val maxGetGames = config.getInt(maxGetGamesConfigPath)
  val dictionaryDir = config.getString(dictionaryDirConfigPath)

  readConfigStringList(languageCodesConfigPath) match {
    case Failure(ex) => throw ex
    case Success(languageCodes) =>
      languageCodes.foreach {
        languageCode =>
          WordDictionary.mkWordDictionary(languageCode, dictionaryDir, MaxMaskedLetters) match {
            case Failure(ex) => throw convertException(ex)
            case Success(dictionary) =>
              logger.info(s"adding language dictionary: ${languageCode}")
              dictionaryCache(languageCode) = dictionary
          }
      }
  }

  private def convertException(ex: Throwable): Exception = {
    ex match {
      // TODO. Should not have to convert. The highest level should know about LanguageExceptions.
      case com.bolour.language.domain.LanguageExceptions.MissingDictionaryException(languageCode, dictionaryDir, ex) =>
        MissingDictionaryException(languageCode, dictionaryDir, ex)
      case _ => InternalAppException("unable to make word dictionary", ex)
    }
  }

  val defaultDb = config.getString(defaultDbPath)
  // val gameDao: GameDao = GameDaoSlick(defaultDb, config)
  // val persister: GamePersister = new GamePersisterJsonImpl(new GameJsonPersisterMemoryImpl(), Version.version)
  val persister: GamePersister = new GamePersisterJsonBridge(GameJsonPersisterSlickImpl(defaultDb, config), Version.version)

  // val seedPlayerName = "You"
  // val seedPlayer = Player(stringId, seedPlayerName)

  migrate()

  override def migrate() = {
    // TODO. Proper migration. This one is for testing only.
    for /* Future */ {
    _ <- persister.migrate()
    } yield ()

  }

  override def reset(): Future[Unit] = {
    persister.clearAllData()
  }

  // TODO. Check params.
  // Dimension >= 5 <= 30.
  // Tray capacity > 0 < 20.
  // language code - needs supported languages.
  // userId not empty.
  override def startGame(gameParams: GameParams, initPieces: InitPieces, maybeEmail: Option[String]): Future[Game] = {
    if (gameCache.size >= maxActiveGames)
      return Future.failed(SystemOverloadedException())

    for {
      maybeUser <-
        maybeEmail match {
          case Some(email) => appService.findEmailUser(email)
          case None => Future.successful(None)
        }
      gameBase = GameBase(gameParams, initPieces, maybeUser.map(_.id))
      game <- Future.fromTry(GameObj.mkGame(gameBase))
      _ <- saveGame(game)
      _ = gameCache.put(gameBase.gameId, game)
    } yield game
  }

  override def checkGameOwner(gameId: String, userEmail: String): Future[Unit] = {

    def checkAccess(allow: Boolean): Future[Unit] =
      if(allow) Future.successful(())
      else Future.failed(AccessViolationException("game", gameId, s"not owned ${userEmail}"))

    for {
      optEmailUser <- appService.findEmailUser(userEmail)
      game <- getGameById(gameId)
      _ = logger.info(s"checkGameOwner - game.state: ${game.state}")
      result <- game.gameBase.playerId match {
        // Game not having a player id indicates the game is a guest game.
        case None => checkAccess(userEmail == EmailConstants.guestEmail)
        case Some(playerId) =>
          // There must be a signed up user for the email.
          val triedEmailUser = optionToTry(optEmailUser) {NotSignedUpException(userEmail)}
          for {
            emailUser <- ftry(triedEmailUser)
            _ <- checkAccess(playerId == emailUser.id)
          } yield ()
      }
    } yield result
  }

  private def getDictionary(languageCode: String): Try[WordDictionary] = {
    val odict = dictionaryCache.get(languageCode)
    if (odict.isEmpty)
      Failure(UnsupportedLanguageException(languageCode))
    else
      Success(odict.get)
  }

  // TODO. Move to the WordDictionary after moving InvalidWordException to the language package.
  private def checkWord(word: String, languageCode: String, dictionary: WordDictionary): Try[Unit] = {
    if (!dictionary.hasWord(word))
      Failure(InvalidWordException(languageCode, word))
    else
      Success(())
  }

  // TODO. Move business logic validations to game where appropriate.
  override def commitPlay(gameId: String, playPieces: List[PlayPiece]): Future[(GameMiniState, List[Piece], List[Point])] = {

    val word = PlayPieceObj.playPiecesToWord(playPieces)

    // TODO. This can be moved to game.

    def gameCommit(game: Game): Try[(Game, List[Piece], List[Point])] = {
      val languageCode = game.gameBase.gameParams.playParams.languageCode
      for {
        dict <- getDictionary(languageCode)
        _ <- checkWord(word, languageCode, dict)
        _ <- game.checkCrossWords(playPieces, dict)
        (newGame, refills, deadPoints) <- game.addWordPlay(UserPlayer, playPieces, findAndSetBoardBlackPoints(dict))
      } yield (newGame, refills, deadPoints)
    }

    for {
      game <- getActiveGame(gameId)
      (newGame, refills, deadPoints) <- Future.fromTry(gameCommit(game))
      _ = gameCache.put(gameId, newGame)
      _ <- saveGame(newGame)
    }
      yield (newGame.miniState, refills, deadPoints)
  }

  def ftry[T](value: Try[T]): Future[T] = Future.fromTry[T](value)

  override def machinePlay(gameId: String): Future[(GameMiniState, List[PlayPiece], List[Point])] = {

    // TODO. Move to Game.
    def bestMatch(dict: WordDictionary, game: Game): List[PlayPiece] = {
      val stripMatcher = new StripMatcher {
        override def dictionary: WordDictionary = dict
        override def board: Board = game.board
        override def tray: Tray = game.trays(playerIndex(MachinePlayer))
      }
      stripMatcher.bestMatch()
    }

    for {
      game <- getActiveGame(gameId)
      languageCode = game.gameBase.gameParams.playParams.languageCode
      dict <- ftry(getDictionary(languageCode))
      playPieces = bestMatch(dict, game)

      (newGame, deadPoints) <- playPieces match {
        case Nil => swapMachinePiece(game) map (newGame => (newGame, Nil))
        case _ =>
          val tried = game.addWordPlay(MachinePlayer, playPieces, findAndSetBoardBlackPoints(dict))
          ftry(tried).map(t => (t._1, t._3)) // Extract new game and dead points from 3-tuple.
      }
    _ = gameCache.put(gameId, newGame)
    _ <- saveGame(newGame)
    } yield (newGame.miniState, playPieces, deadPoints)
  }

  private def swapMachinePiece(game: Game): Future[Game] = {
    val tray = game.tray(MachinePlayer)
    val letter = letterDistribution.leastFrequentValue(tray.letters.toList).get
    val swappedPiece = tray.findPieceByLetter(letter).get
    for {
      (newGame, _) <- Future.fromTry(game.addSwapPlay(swappedPiece, MachinePlayer))
    } yield newGame
  }

  override def swapPiece(gameId: String, piece: Piece): Future[(GameMiniState, Piece)] = {
    for {
      game <- getActiveGame(gameId)
      (newGame, newPiece) <- ftry(game.addSwapPlay(piece, UserPlayer))
      _ = gameCache.put(gameId, newGame)
      _ <- saveGame(newGame)
    }
      yield (newGame.miniState, newPiece)
  }

  // Future. Purge old games to reduce data storage.

  override def endGame(gameId: String): Future[GameSummary] = {
    for {
      game <- getActiveGame(gameId)
      endedGame = game.end()
      _ = gameCache.remove(gameId)
      _ <- saveGame(endedGame)
    }
      yield endedGame.stop().summary()
  }

  // TODO. Add parameter gameState: Option[GameState] and if set - check that game is in that state before action.
  private def actOnGame[R](gameId: ID, action: Game => Future[R]): Future[R] = {
    for {
      game <- getGameById(gameId)
      result <- action(game)
    } yield result
  }

  private def suspendAction(game: Game): Future[Unit] = {
    val suspended = game.suspend()
    gameCache.remove(game.gameBase.gameId)
    saveGame(suspended)
  }

  // TODO. URGENT. Should not be able to suspend guest game. InvalidGuestActionException.
  // TODO. Can only suspend a running game.
  override def suspendGame(gameId: ID): Future[Unit] = {
    actOnGame(gameId, suspendAction);
  }

  private def resignAction(game: Game): Future[Unit] = {
    val resigned = game.resign()
    gameCache.remove(game.gameBase.gameId)
    saveGame(resigned)
  }

  // TODO. Can only resign a running game.
  override def resignGame(gameId: ID): Future[Unit] = {
    actOnGame(gameId, resignAction);
  }

  private def resumeAction(game: Game): Future[Game] = {
    val gameId = game.gameBase.gameId
    if (game.isCompleted) {
      gameCache.remove(gameId)
      return Future.failed(InactiveGameException(gameId))
    }
    val resumed = game.copy(state = GameState.RUNNING)
    gameCache.put(gameId, resumed)
    for {
      _ <- saveGame(resumed)
    } yield resumed
  }

  // TODO. URGENT. Extra safety. Check that game is not a guest game.
  // InvalidGuestActionException.
  override def resumeGame(gameId: ID): Future[Game] = {
    val ogame = Option(gameCache.get(gameId))
    // If it is in the cache, it is not suspended - so nothing to do.
    ogame match {
      case Some(game) => Future.successful(game)
      case None => actOnGame(gameId, resumeAction)
    }
  }

  private def cancelAction(game: Game): Future[Unit] = {
    val gameId = game.gameBase.gameId
    gameCache.remove(gameId)
    persister.deleteGame(gameId)
  }

  override def cancelGame(gameId: ID): Future[Unit] = {
    actOnGame(gameId, cancelAction);
  }

  /*
   * TODO. The entire piece provider data structure is automatically
   *       serialized saved and restored in game. Wasteful.
   *
   * Part of the data is constant, e.g., letter frequencies.
   * The piece provider type is stored as part of gameParams.
   * No need to store the entire piece provider.
   * On retrieval, the correct piece provider can be linked to.
   */
  override def findGameById(gameId: ID): Future[Option[Game]] = {
    // If the game is owned by a guest, it will not be saved in the database.
    val optGame = Option(gameCache.get(gameId))
    if (optGame.isDefined)
      Future.successful(optGame)
    else
      persister.findGameById(gameId)
  }

  private def checkGameActive(game: Game): Future[Unit] = {
    if (game.isRunning)
      Future.successful(())
    else
      Future.failed(InactiveGameException(game.gameBase.gameId))
  }

  private def getActiveGame(gameId: ID): Future[Game] = {
    for {
      game <- getGameById(gameId)
      _ <- checkGameActive(game)
    } yield game
  }

//  private def getActiveGame(gameId: ID): Future[Game] = {
//    for {
//      optGame <- findGameById(gameId)
//      game <- optGame match {
//        case None => Future.failed(MissingGameException(gameId))
//        case Some(game) =>
//          if (game.isRunning)
//            Future.successful(game)
//          else
//            Future.failed(InactiveGameException(gameId))
//      }
//    } yield game
//  }
//
  private def getGameById(gameId: ID): Future[Game] = {
    for {
      optGame <- findGameById(gameId)
      game <- optGame match {
        case None => Future.failed(MissingGameException(gameId))
        case Some(game) => Future.successful(game)
      }
    } yield game
  }

  override def getUserGames(
    email: String,
    fromEpochSecond: Long,
    toEpochSecond: Long,
    maxGames: Int
  ): Future[List[Game]] = {
    // val actualMaxGames: Int = maxGames.foldLeft(maxGetGames)(_ min _)
    val actualMaxGames: Int = maxGetGames.min(maxGames)
    for {
      ouser <- appService.findEmailUser(email)
      games <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.getUserGames(user.id, fromEpochSecond, toEpochSecond, actualMaxGames)
      }
    } yield games
  }

  // TODO. URGENT. Hack. Assuming no more than 1000000 games for now. Use config parameter.
  val maxUserGames = 1000000

  private def getAllUserGames(email: String): Future[List[Game]] = {
    for {
      ouser <- appService.findEmailUser(email)
      games <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.getUserGames(user.id, 0L, nowSecs, maxUserGames)
      }
    } yield games
  }

  def getUnfinishedUserGames(email: String): Future[List[Game]] = {
    for {
      ouser <- appService.findEmailUser(email)
      games <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.getUnfinishedUserGames(user.id)
      }
    } yield games
  }

  private def removeUserGamesFromCache(email: String): Future[Unit] = {
    for {
      games <- getAllUserGames(email)
      _ = games.map(g => gameCache.remove(g.gameBase.gameId))
    } yield ()
  }

  override def saveUserGameSettings(email: String, settings: UserGameSettings): Future[Unit] = {
    for {
      ouser <- appService.findEmailUser(email)
      games <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.saveUserGameSettings(user.id, settings)
      }
    } yield games
  }

  override def getUserGameSettings(email: String): Future[Option[UserGameSettings]] = {
    for {
      ouser <- appService.findEmailUser(email)
      games <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.getUserGameSettings(user.id)
      }
    } yield games
  }

  override def removeAllUserGameRelatedInfo(email: String): Future[Unit] = {
    for {
      ouser <- appService.findEmailUser(email)
      _ <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) =>
          for {
            - <- removeUserGamesFromCache(email)
            - <- persister.removeAllUserGameRelatedInfo(user.id)
          } yield ()
      }
    } yield ()
  }

  def timeoutLongRunningGames(): Future[Unit] = {
    import scala.collection.JavaConverters._
    val nowSecond = nowSecs;
    def aged(gameId: String): Boolean = {
      val ogame = Option(gameCache.get(gameId))
      ogame match {
        case None => false
        case Some(game) =>
          val lastSecond = game.lastSecond
          val gameSeconds = nowSecond - lastSecond
          gameSeconds > (maxGameIdleMinutes * 60)
      }
    }
    val gameIdList = gameCache.keys().asScala.toList
    val longRunningGameIdList = gameIdList filter { aged }
    // logger.info(s"games running more than ${maxGameIdleMinutes}: ${longRunningGameIdList}")
    Future.sequence(longRunningGameIdList.map(suspendGame)).map(_ => ())
  }

  private def saveGame(game: Game) : Future[Unit] = {
    // Games are not saved for guest users.
    // TODO. Provide isGuestGame for game and use it.
    // if (game.gameBase.playerId.isEmpty)
    if (game.isGuestGame)
      Future.successful(());
    else
      persister.saveGame(game)
  }
}

object GameServiceImpl {
  val gameCache: ConcurrentHashMap[String, Game] = new ConcurrentHashMap()
  val dictionaryCache: MutableMap[String, WordDictionary] = MutableMap()

  def cacheGameState(gameId: String, gameState: Game): Try[Unit] = Try {
    gameCache.put(gameId, gameState)
  }

  val letterFrequencies = List(
    ('A', 81),
    ('B', 15),
    ('C', 28),
    ('D', 42),
    ('E', 127),
    ('F', 22),
    ('G', 20),
    ('H', 61),
    ('I', 70),
    ('J', 2),
    ('K', 8),
    ('L', 40),
    ('M', 24),
    ('N', 67),
    ('O', 80),
    ('P', 19),
    ('Q', 1),
    ('R', 60),
    ('S', 63),
    ('T', 91),
    ('U', 28),
    ('V', 10),
    ('W', 23),
    ('X', 2),
    ('Y', 20),
    ('Z', 1)
  )

  val letterDistribution = FrequencyDistribution(letterFrequencies)
  val caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def mkPieceProvider(pieceProviderType: PieceProviderType): PieceProvider = {
    pieceProviderType match {
      case PieceProviderType.Random => RandomPieceProvider(letterDistribution, 0)
      case PieceProviderType.Cyclic => CyclicPieceProvider(caps, 0)
    }
  }
}
