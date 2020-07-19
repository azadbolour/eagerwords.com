--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.BaseServiceFixtures (
    initTest
  , testUserId
  , mkUser
  , testUser
  , testDimension
  , testSquarePixels
  , testTrayCapacity
  , testGameSettings
  , testGameParams
  , testBoardCenterPoint
  ) where

import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig))
import Bolour.Util.DeployEnv (DeployEnv(..))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import EagerWords.Common.Domain.Piece (Piece)
import qualified EagerWords.Common.Domain.Piece as Piece
import Bolour.Plane.Domain.Point (Point, Point(Point))
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Server.Domain.GameCache as GameCache
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import qualified EagerWords.Server.Domain.GameEnv as GameEnv
import qualified Bolour.Language.Domain.WordDictionary as Dict
import qualified Bolour.Language.Domain.DictionaryCache as DictCache
import qualified Bolour.Language.Domain.DictionaryIO as DictIO
import qualified Bolour.Util.PersistRunner as PersistRunner
import Bolour.Util.PersistRunner (ConnectionProvider)
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Bolour.Util.SpecUtil (satisfiesRight)
import EagerWords.Server.Service.GamePersister (GamePersister, GamePersister(GamePersister))
import qualified EagerWords.Server.Service.GamePersister as GamePersister
import qualified EagerWords.Server.Service.GamePersisterJsonBridge as GamePersisterJsonBridge
import qualified EagerWords.Server.Service.GameJsonSqlPersister as GameJsonSqlPersister
import qualified EagerWords.Server.Service.GameJsonSqlPersister as GamePersister
import qualified EagerWords.Server.Domain.ServerVersion as ServerVersion
import EagerWords.Server.Domain.User
import qualified Bolour.Util.MiscUtil as Util
import qualified EagerWords.Server.Service.GameTransformerStack as TransformerStack
import qualified EagerWords.Server.Service.GameService as GameService
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

testConfigPath = "test-data/test-config.yml"

testUserId = "11111111111"
testUser :: User
testUser = User testUserId "You" "You@example.com"

testDimension = 5
testSquarePixels = 50
testTrayCapacity = 3
testPieceProviderType = PieceProviderType.Cyclic
testStartingPlayer = Nothing
testDeviceType = Nothing

center = testDimension `div` 2
testBoardCenterPoint = Point center center

testGameSettings = GameSettings testDimension testSquarePixels testTrayCapacity "tiny"
  testPieceProviderType testStartingPlayer testDeviceType
testPointValues :: [[Int]]
testPointValues = replicate testDimension $ replicate testDimension (1 :: Int)
testGameParams = GameParams testGameSettings testPointValues

mkPersister :: ConnectionProvider -> GamePersister
mkPersister connectionProvider =
  let jsonPersister = GameJsonSqlPersister.mkPersister connectionProvider
      version = ServerVersion.version
  in GamePersisterJsonBridge.mkBridge jsonPersister version

mkUser :: GameEnv -> String -> IO ()
mkUser env name = do
    userId <- Util.mkUuid
    let user = User userId name (name ++ "@example.com")
    eitherUnit <- runExceptT $ TransformerStack.runDefaultUnprotected env $ GameService.saveUserService user
    satisfiesRight eitherUnit

initTest :: IO GameEnv
initTest = do
  serverConfig <- ServerConfig.getServerConfig $ Just testConfigPath
  let ServerConfig {maxActiveGames, dbConfig} = serverConfig
  connectionProvider <- PersistRunner.mkConnectionProvider dbConfig
  let persister @ GamePersister {migrate} = mkPersister connectionProvider
  runExceptT migrate
  runExceptT $ GamePersister.clearAllData persister
  cache <- GameCache.mkGameCache maxActiveGames
  dictionaryDir <- GameEnv.getDictionaryDir "data"
  -- dictionaryCache <- DictCache.mkCache dictionaryDir 100 2
  Right dictionaryCache <- runExceptT $ DictIO.readAllDictionaries dictionaryDir ["tiny"] ServerConfig.maxDictionaries ServerConfig.dictionaryMaxMaskedLetters
  return $ GameEnv serverConfig connectionProvider cache dictionaryCache