{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char (isUpper, toUpper)
import Data.List
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (runLoggingT)

import qualified Bolour.Util.PersistRunner as PersistRunner
import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig), DeployEnv(..))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import EagerWords.Common.Domain.Player(Player, Player(Player))
import EagerWords.Common.Domain.Piece (Piece(Piece))
import Bolour.Plane.Domain.GridValue (GridValue, GridValue(GridValue))
import qualified Bolour.Plane.Domain.GridValue as GridValue
import qualified EagerWords.Common.Domain.GridPiece as GridPiece
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.PlayPiece (PlayPiece, PlayPiece(PlayPiece))
import EagerWords.Server.Domain.GameCache as GameCache
import EagerWords.Server.Service.GameDao (cleanupDb)
import qualified EagerWords.Server.Service.GameDao as GameDao
import EagerWords.Server.Domain.GameError
import EagerWords.Server.Domain.Game (Game(Game))
import EagerWords.Server.Domain.Play (Play(Play))
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import EagerWords.Server.Service.GameTransformerStack
import qualified Bolour.Language.Domain.WordDictionary as Dict
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Domain.GridPiece (GridPiece)
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import qualified EagerWords.Server.Domain.GameEnv as GameEnv

import qualified EagerWords.Server.Domain.Play as Play (playToWord)
import qualified EagerWords.Common.Domain.Piece as Piece
import qualified EagerWords.Server.Domain.Tray as Tray
import qualified EagerWords.Server.Domain.Game as Game
import qualified EagerWords.Server.Domain.Board as Board
import EagerWords.Server.Service.GameService (
    saveUserService
  , commitPlayService
  , startGameService
  , machinePlayService
  , swapPieceService
  , getGamePlayDetailsService
  )
-- TODO. Should not depend on higher level module.
-- import EagerWords.Util.TestUtil (mkInitialPlayPieces)
-- import qualified EagerWords.Server.Service.ServiceTestFixtures as Fixtures
import qualified Bolour.Language.Domain.DictionaryCache as DictCache

testConfigPath = "test-data/test-config.yml"
thePlayer = "You"
testDimension = 5
center = testDimension `div` 2
testTrayCapacity = 3
pieceProviderType = PieceProviderType.Cyclic

gameParams = GameParams testDimension testTrayCapacity Dict.defaultLanguageCode thePlayer pieceProviderType

centerGridPoint = Point center center

centerGridPiece :: Char -> IO GridPiece
centerGridPiece value = do
  piece <- Piece.mkPiece value
  return $ GridValue piece centerGridPoint

initTest :: IO GameEnv
initTest = do
  serverConfig <- ServerConfig.getServerConfig $ Just testConfigPath
  let ServerConfig {maxActiveGames, dbConfig} = serverConfig
  connectionProvider <- PersistRunner.mkConnectionProvider dbConfig
  GameDao.migrateDb connectionProvider
  GameDao.cleanupDb connectionProvider
  cache <- GameCache.mkGameCache maxActiveGames
  dictionaryDir <- GameEnv.getDictionaryDir ""
  dictionaryCache <- DictCache.mkCache dictionaryDir 100 2
  return $ GameEnv serverConfig connectionProvider cache dictionaryCache


main :: IO ()

main = do
  print "hello"
  word <- runner'' $ do
    saveUserService $ Player thePlayer
    Game {gameId} <- startGameService gameParams [] [] [] []
    (miniState, playedPieces, deadPieces) <- machinePlayService gameId
    let word = Play.playToWord $ Play playedPieces
    return word
  print word

printx :: String -> ExceptT GameError IO ()
printx s = do
  liftIO $ print s
  return ()

-- TODO. Annotate spec do statements with the demystified type of their monad.
-- TODO. Factor out common test functions to a base type class.

-- TODO. Test with games of dimension 1 as a boundary case.

runner :: GameEnv -> GameTransformerStack a -> IO (Either GameError a)
runner env stack = runExceptT $ flip runLoggingT printx $ runReaderT stack env

-- TODO. How to catch Left - print error and return gracefully.
runner' env stack = do
  Right val <- runner env stack
  return val

runner'' :: GameTransformerStack a -> IO a
runner'' stack = do
  env <- initTest
  runner' env stack


