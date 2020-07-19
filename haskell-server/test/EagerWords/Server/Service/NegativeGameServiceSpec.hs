--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Server.Service.NegativeGameServiceSpec (
    spec
  ) where

import Data.Char (toUpper)
import Test.Hspec
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (runLoggingT)

import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import EagerWords.Common.Domain.GameParams (GameParams(..))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import qualified Bolour.Plane.Domain.Axis as Axis (Axis(..))
import qualified Bolour.Util.PersistRunner as PersistRunner
import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig))
import Bolour.Util.DeployEnv (DeployEnv(..))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import EagerWords.Server.Domain.GameCache as GameCache
import EagerWords.Server.Domain.GameError (GameError)

import qualified EagerWords.Server.Service.GameService as GameService

import qualified EagerWords.Server.Domain.GameError as GameError
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import EagerWords.Server.Domain.User
import EagerWords.Server.Service.GameTransformerStack
import qualified Bolour.Language.Domain.DictionaryCache as DictCache

import qualified EagerWords.Server.Service.ServiceTestFixtures as Fixtures
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

printx :: String -> ExceptT GameError IO ()
printx s = do
  liftIO $ print s
  return ()

runner :: GameEnv -> GameTransformerStack a -> IO (Either GameError a)
runner env stack = runExceptT $ flip runLoggingT printx $ runReaderT stack env

runR :: GameEnv -> GameTransformerStack a -> IO a
runR env stack = do
  Right val <- runner env stack
  return val

runL :: GameEnv -> GameTransformerStack a -> IO GameError
runL env stack = do
  Left error <- runner env stack
  return error

runR' :: GameTransformerStack a -> IO a
runR' stack = do
  env <- Fixtures.initTest
  runR env stack

runL' :: GameTransformerStack a -> IO GameError
runL' stack = do
  env <- Fixtures.initTest
  runL env stack


-- TODO. Annotate spec do statements with the demystified type of their monad.
-- TODO. Factor out common test functions to a base type class.

nonExistentUserId = ""
nonExistentPlayerName = "Mary"
-- paramsBadPlayer = Fixtures.testGameParams {GameParams.userId = nonExistentPlayerName}
nonAlphaNumPlayerName = "Mary-?"
-- paramsNonAlphaNumPlayer = Fixtures.testGameParams {GameParams.userId = nonAlphaNumPlayerName}
badDimension = -1
settingsBadDimension :: GameSettings
settingsBadDimension = Fixtures.testGameSettings {GameSettings.dimension = badDimension}
paramsBadDimension = Fixtures.testGameParams {settings = settingsBadDimension}
settingsZeroWidth = Fixtures.testGameSettings {GameSettings.dimension = 0}
paramsZeroWidth = Fixtures.testGameParams {settings = settingsZeroWidth}
badTrayCapacity = 0
settingsBadTrayCapacity :: GameSettings
settingsBadTrayCapacity = Fixtures.testGameSettings {GameSettings.trayCapacity = badTrayCapacity}
paramsBadTrayCapacity = Fixtures.testGameParams {settings = settingsBadTrayCapacity}
-- TODO. 0 and 1 are also bad sizes.
initPieces = InitPieces [] [] []

spec :: Spec
spec =
  describe "invalid data to start a game" $ do
    it "guards against non-existent player" $
      do
        runR' $ GameService.saveUserService $ Fixtures.testUser
        error <- runL' $ GameService.startGameService Fixtures.testGameParams initPieces nonExistentUserId
        error `shouldBe` GameError.MissingPlayerError nonExistentUserId

    it "disallows negative board dimensions" $
      do
        runR' $ GameService.saveUserService $ Fixtures.testUser
        error <- runL' $ GameService.startGameService paramsBadDimension initPieces Fixtures.testUserId
        error `shouldBe` GameError.InvalidDimensionError badDimension

    it "disallows 0 board dimensions" $
      do
        runR' $ GameService.saveUserService $ Fixtures.testUser
        error <- runL' $ GameService.startGameService paramsZeroWidth initPieces Fixtures.testUserId
        error `shouldBe` GameError.InvalidDimensionError 0

    it "disallows trays with 0 capacity" $
      do
        runR' $ GameService.saveUserService $ Fixtures.testUser
        error <- runL' $ GameService.startGameService paramsBadTrayCapacity initPieces Fixtures.testUserId
        error `shouldBe` GameError.InvalidTrayCapacityError badTrayCapacity


