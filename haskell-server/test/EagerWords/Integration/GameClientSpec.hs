--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

-- See https://docs.servant.dev/en/stable/cookbook/testing/Testing.html.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Integration.GameClientSpec (
    spec
  ) where

import Data.Char (isUpper, toUpper)
import Data.Maybe (fromJust)
import Data.List
import Control.Concurrent (ThreadId, killThread)
import Control.Monad.Except (runExceptT)
import Test.Hspec
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant
import Servant.Client
import Servant.Server
import Network.Wai (Application)

import EagerWords.Common.GameApi (GameApi, gameApi)
import EagerWords.Common.Domain.Piece (Piece(Piece))
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import qualified EagerWords.Common.Domain.Piece as Piece
import EagerWords.Common.Domain.PlayPiece (PlayPiece(PlayPiece))
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece

import Bolour.Plane.Domain.Point (Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.GameParams (GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Domain.UserDto (UserDto(UserDto))
import EagerWords.Common.Message.SwapPieceResponse (SwapPieceResponse(..))
import qualified EagerWords.Common.Message.StartGameResponse as StartGameResponse
import EagerWords.Common.Message.StartGameRequest (StartGameRequest(StartGameRequest))
import EagerWords.Common.Message.CommitPlayResponse (CommitPlayResponse, CommitPlayResponse(CommitPlayResponse))
import qualified EagerWords.Common.Message.CommitPlayResponse as CommitPlayResponse
import EagerWords.Common.Message.MachinePlayResponse (MachinePlayResponse, MachinePlayResponse(MachinePlayResponse))
import qualified EagerWords.Common.Message.MachinePlayResponse as MachinePlayResponse

import qualified EagerWords.Server.Web.GameEndPoint as GameEndPoint
import qualified EagerWords.Util.TestUtil as TestUtil
import qualified Bolour.Util.SpecUtil as SpecUtil
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import qualified EagerWords.Server.Domain.GameEnv as GameEnv

import qualified Bolour.Util.PersistRunner as PersistRunner
import Bolour.Util.PersistRunner (ConnectionProvider)
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig

import EagerWords.Server.Domain.GameEnv (GameEnv(GameEnv))
import qualified EagerWords.Client.GameClient as Client
import qualified Bolour.Language.Domain.WordDictionary as Dict
import qualified EagerWords.Server.Domain.GameCache as GameCache
import qualified Bolour.Language.Domain.DictionaryCache as DictCache
import qualified Bolour.Language.Domain.DictionaryIO as DictIO
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType

import EagerWords.Server.Service.GamePersister (GamePersister, GamePersister(GamePersister))
import qualified EagerWords.Server.Service.GamePersister as GamePersister
import qualified EagerWords.Server.Service.GamePersisterJsonBridge as GamePersisterJsonBridge
import qualified EagerWords.Server.Service.GameJsonSqlPersister as GameJsonSqlPersister
import qualified EagerWords.Server.Service.GameJsonSqlPersister as GamePersister
import qualified EagerWords.Server.Domain.ServerVersion as ServerVersion
import EagerWords.Server.Web.GameEndPoint(mkGameApp)

import qualified Network.Wai.Handler.Warp as Warp

import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

mkPersister :: ConnectionProvider -> GamePersister
mkPersister connectionProvider =
  let jsonPersister = GameJsonSqlPersister.mkPersister connectionProvider
      version = ServerVersion.version
  in GamePersisterJsonBridge.mkBridge jsonPersister version

testConfigPath = "test-data/test-config.yml"

getGameEnv :: IO GameEnv
getGameEnv = do
  serverConfig <- ServerConfig.getServerConfig $ Just testConfigPath
  let ServerConfig.ServerConfig {maxActiveGames, dbConfig} = serverConfig
  connectionProvider <- PersistRunner.mkConnectionProvider dbConfig
  let persister @ GamePersister {migrate} = mkPersister connectionProvider
  runExceptT migrate
  runExceptT $ GamePersister.clearAllData persister
  cache <- GameCache.mkGameCache maxActiveGames
  dictionaryDir <- GameEnv.getDictionaryDir "data"
  -- dictionaryCache <- DictCache.mkCache dictionaryDir 100 2
  Right dictionaryCache <- runExceptT $ DictIO.readAllDictionaries dictionaryDir ["tiny"] 100 2
  return $ GameEnv serverConfig connectionProvider cache dictionaryCache

getGameApp :: IO Application
getGameApp = do
  env <- getGameEnv
  mkGameApp env

withGameApp :: (Warp.Port -> IO ()) -> IO ()
withGameApp action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication getGameApp action
  -- Warp.testWithApplication (pure userApp) action

-- TODO. Duplicated in WebTestFixtures. Unify into single fixture module.

dimension = 9
center = dimension `div` 2

pointValues :: [[Int]]
pointValues = replicate dimension $ replicate dimension (1 :: Int)

userId = "222255555"
name = "John"
testUser = UserDto userId "You" "You@example.com"

testPieceProviderType = PieceProviderType.Cyclic
params = GameParams (GameSettings dimension 50 12 "tiny" testPieceProviderType Nothing Nothing) pointValues

testBoardCenterPoint = Point center center

-- TODO. End duplicated

spec :: Spec
spec = around withGameApp $ do
  -- let gameClient = client (Proxy :: Proxy GameApi)
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

  describe "start a game and make a player play and a machine play" $
    it "start a game and make a player play and a machine play" $ \port -> do

      -- eitherMaybeUnit <- runExceptT (Client.addPlayer (PlayerDto testUser) manager baseUrl)
      eitherMaybeUnit <- runClientM (Client.addUser testUser) (clientEnv port)

      let uPieces = [Piece 'B' "1", Piece 'E' "2", Piece 'T' "3"] -- Allow the word 'BET'
          mPieces = [Piece 'S' "4", Piece 'T' "5", Piece 'Z' "6"] -- Allow the word 'SET' across.
          initPieces = InitPieces [] uPieces mPieces

      StartGameResponse.StartGameResponse {gameId, trayPieces} <- SpecUtil.satisfiesRight
        =<< runClientM (Client.startGame (StartGameRequest params initPieces userId)) (clientEnv port)

      let pc0:pc1:pc2:_ = uPieces
          center = dimension `div` 2
          playPieces = [
              PlayPiece pc0 (Point center (center - 1)) True
            , PlayPiece pc1 (Point center center) True
            , PlayPiece pc2 (Point center (center + 1)) True
            ]

      CommitPlayResponse {gameMiniState, replacementPieces} <- SpecUtil.satisfiesRight
        =<< runClientM (Client.commitPlay gameId playPieces) (clientEnv port)
      length replacementPieces `shouldBe` 3
      MachinePlayResponse {gameMiniState, playedPieces} <- SpecUtil.satisfiesRight
        =<< runClientM (Client.machinePlay gameId) (clientEnv port)
      print $ PlayPiece.playPiecesToWord playedPieces
      let piece = last trayPieces
      SwapPieceResponse {gameMiniState, piece = swappedPiece} <- SpecUtil.satisfiesRight
         =<< runClientM (Client.swapPiece gameId piece) (clientEnv port)
      let Piece {value} = swappedPiece
      value `shouldSatisfy` isUpper
      return ()


