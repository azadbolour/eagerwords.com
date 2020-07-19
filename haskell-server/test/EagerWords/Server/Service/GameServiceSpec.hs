--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.GameServiceSpec (
    spec
  ) where

import Test.Hspec
import Data.Char (isUpper, toUpper)
import Data.List
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (runLoggingT)

import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import qualified Bolour.Util.PersistRunner as PersistRunner
import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig))
import Bolour.Util.DeployEnv (DeployEnv(..))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import EagerWords.Common.Domain.Piece (Piece(Piece))
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.PlayPiece (PlayPiece, PlayPiece(PlayPiece))
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece
import EagerWords.Server.Domain.GameCache as GameCache
import EagerWords.Server.Domain.GameError
import EagerWords.Server.Domain.Game (Game(Game))
-- import EagerWords.Server.Domain.Play (Play(Play))
import EagerWords.Server.Domain.GameEnv (GameEnv, GameEnv(GameEnv))
import EagerWords.Server.Service.GameTransformerStack

-- import qualified EagerWords.Server.Domain.Play as Play (playToWord)
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
  )
-- TODO. Should not depend on higher level module.
import EagerWords.Util.TestUtil (mkInitialPlayPieces)
import qualified EagerWords.Server.Service.ServiceTestFixtures as Fixtures
import qualified Bolour.Language.Domain.DictionaryCache as DictCache

initPieces = InitPieces [] [] []

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
  env <- Fixtures.initTest
  runner' env stack

spec :: Spec
spec = do
  describe "start a game" $
    it "starts game" $
      do -- IO
        userTray <- runner'' $ do -- GameTransformerStack
          saveUserService $ Fixtures.testUser
          Game {trays} <- startGameService Fixtures.testGameParams initPieces Fixtures.testUserId
          return $ trays !! 0
        length (Tray.pieces userTray) `shouldSatisfy` (== Fixtures.testTrayCapacity)

  describe "commits a play" $
    it "commit a play" $
      do -- IO
        let uPieces = [Piece 'B' "1", Piece 'E' "2", Piece 'T' "3"] -- Allow the word 'BET'
            mPieces = [Piece 'S' "4", Piece 'T' "5", Piece 'Z' "6"] -- Allow the word 'SET' across.
        (miniState, replacementPieces, deadPieces) <- runner'' $ do -- GameTransformerStack
          saveUserService $ Fixtures.testUser
          game @ Game {board, trays} <- startGameService Fixtures.testGameParams (InitPieces [] uPieces mPieces) Fixtures.testUserId
          let pc0:pc1:pc2:_ = uPieces
              center = Fixtures.testDimension `div` 2
              playPieces = [
                  PlayPiece pc0 (Point center (center - 1)) True
                , PlayPiece pc1 (Point center center) True
                , PlayPiece pc2 (Point center (center + 1)) True
                ]
          commitPlayService (Game.gameId game) playPieces -- refills
        length replacementPieces `shouldBe` 3

  describe "make machine play" $
    it "make machine play" $
      do -- IO
        word <- runner'' $ do
          saveUserService $ Fixtures.testUser
          game <- startGameService Fixtures.testGameParams initPieces Fixtures.testUserId
          let gameId = Game.gameId game
          (miniState, playedPieces, deadPieces) <- machinePlayService gameId
          let word = PlayPiece.playPiecesToWord playedPieces
          return word
        print word
        length word `shouldSatisfy` (> 1)

  describe "swap a piece" $
    it "swap a piece" $
      do
        value <- runner'' $ do
          saveUserService $ Fixtures.testUser
          game @ Game {trays} <- startGameService Fixtures.testGameParams initPieces Fixtures.testUserId
          let gameId = Game.gameId game
          let userTray = trays !! 0
              piece = head (Tray.pieces userTray)
          -- TODO satisfiesRight
          (miniState, Piece {value}) <- swapPieceService gameId piece
          return value
        value `shouldSatisfy` isUpper
